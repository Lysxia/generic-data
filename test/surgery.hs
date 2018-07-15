{-# LANGUAGE
    CPP,
    DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    TypeApplications,
    TypeOperators #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- Many of these tests are more about ensuring things typecheck than really
-- comparing their runtime results.

import Data.Bifunctor (second)
import Data.Functor.Identity
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Generic.Data.Surgery
import Generic.Data.Types (Data(Data))

data T = A | B Int | C Int Int Int Int Int deriving (Eq, Show, Generic)

data P = P Int Int Int deriving (Eq, Show, Generic)
data R = R { u, v, w :: Int } deriving (Eq, Show, Generic)

main :: IO ()
main = defaultMain test

show' :: Show (f ()) => f () -> String
show' = show

unit :: f () -> f ()
unit = id

test :: TestTree
test = testGroup "surgery"
  [ testRoundtrip
  , testConsumer
  , testProducer
  ]

rt :: (Eq a, Show a) => a -> (a -> a) -> Assertion
rt x f = x @?= f x

testRoundtrip :: TestTree
testRoundtrip = testGroup "roundtrip"
  [ testCase "to-from" $ rt (C 1 2 3 4 5) (fromOR . toOR)
  , testCase "CField-rmv-ins" $
      rt (P 1 2 3) (fromOR . insertCField @1 . removeCField @1 . toOR)
  , testCase "CField-ins-rmv" $
      rt ((), P 1 2 3) (fmap fromOR . removeCField @1 . insertCField @1 . fmap toOR)
  , testCase "RField-rmv-ins" $
      rt (R 1 2 3) (fromOR . insertRField @"u" . removeRField @"u" . toOR)
  , testCase "RField-ins-rmv" $
      rt ((), R 1 2 3) (fmap fromOR . removeRField @"t" . insertRField @"t" @1 . fmap toOR)
-- Type error on 8.0
#if __GLASGOW_HASKELL__ >= 802
  , testCase "Constr-rmv-ins" $
      rt A (fromOR . insertConstrT @"A" . removeConstrT @"A" . toOR)
#endif
  , testCase "Constr-ins-rmv" $
      rt (Right A)
         (fmap fromOR . removeConstrT @"Z" . insertConstrT @"Z" @() @0 . fmap toOR)
  ]

testConsumer :: TestTree
testConsumer = testGroup "consumer"
  [ testCase "removeCField" $
      "P 1 3" @?=
      (show' . fromOR' . snd . removeCField @1 . toOR) (P 1 2 3)

  , testCase "removeRField" $
      "R {u = 1, w = 3}" @?=
      (show' . fromOR' . snd . removeRField @"v" . toOR) (R 1 2 3)

  , testCase "insertCField" $
      "P 1 () 2 3" @?=
      (show' . fromOR' . insertCField' @1 () . toOR) (P 1 2 3)

  , testCase "insertRField" $
      "R {u = 1, n = (), v = 2, w = 3}" @?=
      (show' . fromOR' . insertRField' @"n" @1 () . toOR) (R 1 2 3)

-- Loops on 8.0
#if __GLASGOW_HASKELL__ >= 802
    -- N.B. Identity (for constructor B) is inferred.
  , testCase "removeConstr" $
      "[Right A,Left (Identity 0),Right (C 1 2 3 4 5)]" @?=
      (show . fmap (second (unit . fromOR') . removeConstrT @"B" . toOR))
        [A, B 0, C 1 2 3 4 5]

  , testCase "insertConstr" $
      "B 0" @?= (show . fromOR @T . insertConstrT @"B" . Left) (Identity 0)
#endif
  ]

testProducer :: TestTree
testProducer = testGroup "producer"
  [ testCase "removeCField" $
      P 0 0 0 @?=
        (fromOR . snd . removeCField @1 @[Int] . toOR') def

  , testCase "removeRField" $
      R 0 0 0 @?=
        (fromOR . snd . removeRField @"v" @1 @[Int] . toOR') def

  , testCase "insertCField" $
      P 0 9 0 @?=
        (fromOR . insertCField' @1 9 . toOR') def

  , testCase "insertCField" $
      R 0 9 0 @?=
        (fromOR . insertRField' @"v" 9 . toOR') def

  , testCase "removeConstr" $
      Right A @?=
        (fmap fromOR . removeConstrT @"D" @() @3 . toOR') def

    -- N.B. () (for constructor A) is inferred.
  , testCase "insertConstr" $
      B 0 @?=
        (fromOR . insertConstrT @"A" . Right . toOR') def
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
