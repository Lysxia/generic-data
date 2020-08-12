{-# LANGUAGE
    DeriveAnyClass,
    DeriveGeneric,
    DerivingVia,
    DerivingStrategies,
    FlexibleInstances,
    ScopedTypeVariables,
    StandaloneDeriving,
    TypeApplications
  #-}

import Data.Semigroup (Sum(..))
import Text.Show (showParen, showString)

import Control.DeepSeq
import Criterion.Main

import Generic.Data
import Generic.Data.Microsurgery

data H  -- handwritten
data G  -- generic
data S  -- surgery

data T x = C { _a :: Sum Int, _b :: [Int] }
  deriving stock Generic

deriving via (Surgery Derecordify (T S)) instance Show (T S)

instance Show (T H) where
  showsPrec n (C a b) =
    showParen (n > 10)
      (showString "C "
        . showsPrec 11 a
        . showString " "
        . showsPrec 11 b)

deriving via (Generically (T G)) instance Semigroup (T G)
instance Semigroup (T H) where
  C a1 b1 <> C a2 b2 = C (a1 <> a2) (b1 <> b2)

deriving anyclass instance NFData (T G)

instance NFData (T H) where
  rnf (C a b) = rnf a `seq` rnf b `seq` ()

u :: forall x. T x
u = C 33 [99]

v :: forall x. T x
v = C 13 [14]

main :: IO ()
main = defaultMain
  [ bgroup "Show"
      [ bench "handwri" (nf show (u @H))
      , bench "surgery" (nf show (u @S))
      ]
  , bgroup "NFData"
      [ bench "handwri" (nf id (u @H))
      , bench "generic" (nf id (u @G))
      ]
  , bgroup "Semigroup"
      [ bench "baselin" (nf (uncurry (++)) ([99], [14 :: Int]))
      , bench "handwri" (nf (uncurry (<>)) (u @H, v))
      , bench "generic" (nf (uncurry (<>)) (u @G, v))
      ]
  ]
