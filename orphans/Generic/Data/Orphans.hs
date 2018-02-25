{-# OPTIONS_GHC -Wno-orphans #-}

module Generic.Data.Orphans where

import GHC.Generics

instance Monoid c => Applicative (K1 i c) where
  pure _ = K1 mempty
  K1 a <*> K1 b = K1 (mempty a b)
