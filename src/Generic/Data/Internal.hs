{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Data.Internal where

import Data.Functor.Contravariant (Contravariant, phantom)
import GHC.Generics

import Generic.Data.Enum
import Generic.Data.Show

-- | A wrapper to view a generic 'Rep' as the datatype it's supposed
-- to represent, without needing a declaration.
--
-- This can be used to derive types from generic types, and get some instances
-- for free, in particular 'Generic', 'Show', 'Enum', 'Bounded'.
newtype Data r p = Data { unData :: r p }
  deriving (Functor, Foldable, Traversable, Contravariant, Eq, Ord)

instance (Functor r, Contravariant r) => Generic (Data r p) where
  type Rep (Data r p) = r
  to = Data . phantom
  from = phantom . unData

instance Generic1 (Data r) where
  type Rep1 (Data r) = r
  to1 = Data
  from1 = unData

instance GShow r => Show (Data r p) where
  showsPrec = flip (gPrecShows . unData)

instance GEnum r => Enum (Data r p) where
  toEnum = Data . gToEnum
  fromEnum = gFromEnum . unData

instance GEnum r => Bounded (Data r p) where
  minBound = Data gMinBound
  maxBound = Data gMaxBound
