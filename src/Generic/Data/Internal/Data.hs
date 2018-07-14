-- | Generic representations as data types.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Data.Internal.Data where

import Control.Applicative
import Control.Monad
import Data.Functor.Classes
import Data.Functor.Contravariant (Contravariant, phantom)
import Data.Semigroup
import GHC.Generics

import Generic.Data.Internal.Enum
import Generic.Data.Internal.Show

-- | Synthetic data type.
--
-- A wrapper to view a generic 'Rep' as the datatype it's supposed to
-- represent, without needing a declaration.
--
-- This can be used to derive types from generic types, and get some instances
-- for free, in particular 'Generic', 'Show', 'Enum', 'Bounded'.
newtype Data r p = Data { unData :: r p }
  deriving ( Functor, Foldable, Traversable, Applicative, Alternative
           , Monad, MonadPlus, Contravariant
           , Eq, Ord, Eq1, Ord1, Semigroup, Monoid )

-- | Conversion between a generic type and the synthetic type made using its
-- representation.
toData :: Generic a => a -> Data (Rep a) p
toData = Data . from

-- | Inverse of 'fromData'.
fromData :: Generic a => Data (Rep a) p -> a
fromData = to . unData

instance (Functor r, Contravariant r) => Generic (Data r p) where
  type Rep (Data r p) = r
  to = Data . phantom
  from = phantom . unData

instance Generic1 (Data r) where
  type Rep1 (Data r) = r
  to1 = Data
  from1 = unData

instance (GShow1 r, Show p) => Show (Data r p) where
  showsPrec = flip (gLiftPrecShows showsPrec showList . unData)

instance GShow1 r => Show1 (Data r) where
  liftShowsPrec = (fmap . fmap) (flip . (. unData)) gLiftPrecShows

instance GEnum r => Enum (Data r p) where
  toEnum = Data . gToEnum
  fromEnum = gFromEnum . unData

instance GBounded r => Bounded (Data r p) where
  minBound = Data gMinBound
  maxBound = Data gMaxBound
