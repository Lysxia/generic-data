{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Orphans where

import Data.Semigroup
import GHC.Generics

instance Monoid c => Applicative (K1 i c) where
  pure _ = K1 mempty
  K1 a <*> K1 b = K1 (mempty a b)

instance Semigroup (V1 p) where
  v <> _ = v

instance Semigroup (U1 p) where
  _ <> _ = U1

instance Monoid (U1 p) where
  mempty = U1
  mappend = (<>)

deriving instance Semigroup c => Semigroup (K1 i c p)
deriving instance Monoid c => Monoid (K1 i c p)

deriving instance Semigroup (f p) => Semigroup (M1 i c f p)
deriving instance Monoid (f p) => Monoid (M1 i c f p)

instance (Semigroup (f p), Semigroup (g p)) => Semigroup ((f :*: g) p) where
  (x1 :*: y1) <> (x2 :*: y2) = (x1 <> x2) :*: (y1 <> y2)

instance (Monoid (f p), Monoid (g p)) => Monoid ((f :*: g) p) where
  mempty = mempty :*: mempty
  mappend (x1 :*: y1) (x2 :*: y2) = mappend x1 x2 :*: mappend y1 y2

deriving instance Semigroup p => Semigroup (Par1 p)
deriving instance Monoid p => Monoid (Par1 p)

deriving instance Semigroup (f p) => Semigroup (Rec1 f p)
deriving instance Monoid (f p) => Monoid (Rec1 f p)

deriving instance Semigroup (f (g p)) => Semigroup ((f :.: g) p)
deriving instance Monoid (f (g p)) => Monoid ((f :.: g) p)
