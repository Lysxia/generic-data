{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    TypeApplications #-}

import GHC.Generics (Generic(..))
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data.Internal.Surgery

data T = A | B Int | C Int Int Int Int Int deriving (Eq, Show, Generic)

x :: T
x = C 1 2 3 4 5

data P = P Int Int Int deriving (Eq, Show, Generic)
data R = R { u, v, w :: Int } deriving (Eq, Show, Generic)

main :: IO ()
main = defaultMain test

show' :: Show (f ()) => f () -> String
show' = show

test :: TestTree
test = testGroup "surgery"
  [ testCase "roundtrip" $ x @?= (fromLoL . toLoL) x

  , testCase "removeCField" $
      "P 1 3" @?=
      (show' . toData . snd . removeCField @1 . toLoL) (P 1 2 3)

  , testCase "removeRField" $
      "R {u = 1, w = 3}" @?=
      (show' . toData . snd . removeRField @"v" . toLoL) (R 1 2 3)

  , testCase "insertCField" $
      "P 1 () 2 3" @?=
      (show' . toData . insertCField @1 () . toLoL) (P 1 2 3)

  , testCase "insertRField" $
      "R {u = 1, n = (), v = 2, w = 3}" @?=
      (show' . toData . insertRField @"n" @1 () . toLoL) (R 1 2 3)
  ]
