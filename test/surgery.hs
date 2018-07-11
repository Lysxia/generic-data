{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Bifunctor (bimap)
import GHC.Generics (Generic(..))
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data.Surgery

data T = A | B Int | C Int Int Int Int Int deriving (Eq, Show, Generic)

x :: T
x = C 1 2 3 4 5

data P = P Int Int Int deriving (Eq, Show, Generic)
data R = R { u, v, w :: Int } deriving (Eq, Show, Generic)

newtype I a = I { unI :: a } deriving (Eq, Show, Generic)

main :: IO ()
main = defaultMain test

show' :: Show (f ()) => f () -> String
show' = show

unit :: f () -> f ()
unit = id

test :: TestTree
test = testGroup "surgery"
  [ testCase "roundtrip" $ x @?= (fromOR . toOR) x

  , testCase "removeCField" $
      "P 1 3" @?=
      (show' . toData . snd . removeCField @1 . toOR) (P 1 2 3)

  , testCase "removeRField" $
      "R {u = 1, w = 3}" @?=
      (show' . toData . snd . removeRField @"v" . toOR) (R 1 2 3)

  , testCase "insertCField" $
      "P 1 () 2 3" @?=
      (show' . toData . insertCField @1 () . toOR) (P 1 2 3)

  , testCase "insertRField" $
      "R {u = 1, n = (), v = 2, w = 3}" @?=
      (show' . toData . insertRField @"n" @1 () . toOR) (R 1 2 3)

  , testCase "removeConstr" $
      "[Right A,Left 0,Right (C 1 2 3 4 5)]" @?=
      (show . fmap (bimap unI (unit . toData) . removeConstr @"B" . toOR))
        [A, B 0, x]

  , testCase "insertConstr" $
      "B 0" @?= (show . fromOR @T . insertConstr @"B" . Left) (I 0)
  ]
