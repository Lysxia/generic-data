{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    TypeApplications,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Data.Bifunctor (bimap)
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data.Surgery
import Generic.Data.Types (Data(Data))

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
  , testConsumer
  , testProducer
  ]

testConsumer :: TestTree
testConsumer = testGroup "consumer"
  [ testCase "removeCField" $
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

testProducer :: TestTree
testProducer = testGroup "producer"
  [ testCase "removeCField" $
      P 0 0 0 @?=
        (fromOR . snd . removeCField @1 @[Int] . fromData) def

  , testCase "removeRField" $
      R 0 0 0 @?=
        (fromOR . snd . removeRField @"v" @1 @[Int] . fromData) def

  , testCase "insertCField" $
      P 0 9 0 @?=
        (fromOR . insertCField @1 9 . fromData) def

  , testCase "insertCField" $
      R 0 9 0 @?=
        (fromOR . insertRField @"v" 9 . fromData) def

  , testCase "removeConstr" $
      Right A @?=
        (fmap fromOR . removeConstr @"D" @() @3 . fromData) def

  , testCase "insertConstr" $
      B 0 @?=
        (fromOR . insertConstr @"A" @() . Right . fromData) def
  ]

class Def a where
  def :: a

instance Def Int where
  def = 0

instance Def [a] where
  def = []

instance GDef f => Def (Data f x) where
  def = Data gdef

class GDef f where
  gdef :: f x

instance GDef f => GDef (M1 i c f) where
  gdef = M1 gdef

instance GDef f => GDef (f :+: g) where
  gdef = L1 gdef

instance (GDef f, GDef g) => GDef (f :*: g) where
  gdef = gdef :*: gdef

instance Def a => GDef (K1 i a) where
  gdef = K1 def

instance GDef U1 where
  gdef = U1
