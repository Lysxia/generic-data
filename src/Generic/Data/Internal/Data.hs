{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Generic representations as data types.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

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
newtype Data r p = Data { unData :: r p }
  deriving ( Functor, Foldable, Traversable, Applicative, Alternative
           , Monad, MonadPlus, Contravariant
           , Eq, Ord, Eq1, Ord1, Semigroup, Monoid )

-- | Conversion between a generic type and the synthetic type made using its
-- representation. Inverse of 'fromData'.
toData :: Generic a => a -> Data (Rep a) p
toData = Data . from

-- | Inverse of 'toData'.
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

instance GEnum StandardEnum r => Enum (Data r p) where
  toEnum = Data . gToEnum @StandardEnum
  fromEnum = gFromEnum @StandardEnum . unData

instance GBounded r => Bounded (Data r p) where
  minBound = Data gMinBound
  maxBound = Data gMaxBound
