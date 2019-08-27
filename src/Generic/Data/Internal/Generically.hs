{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Newtypes with instances implemented using generic combinators.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Generically where

import Control.Applicative
import Data.Functor.Classes
import Data.Semigroup
import GHC.Generics

import Generic.Data.Internal.Prelude
import Generic.Data.Internal.Enum
import Generic.Data.Internal.Error
import Generic.Data.Internal.Show

-- | Type with instances derived via 'Generic'.
newtype Generically a = Generically { unGenerically :: a }

instance Generic a => Generic (Generically a) where
  type Rep (Generically a) = Rep a
  to = Generically . to
  from = from . unGenerically

instance (Generic a, Eq (Rep a ())) => Eq (Generically a) where
  (==) = geq

instance (Generic a, Ord (Rep a ())) => Ord (Generically a) where
  compare = gcompare

instance (Generic a, GShow0 (Rep a)) => Show (Generically a) where
  showsPrec = gshowsPrec

-- |
-- >>> :set -XDeriveGeneric -XDerivingVia
-- >>> :{
--   data AB = A | B
--     deriving stock Generic
--     deriving Semigroup via Generically AB
-- :}
-- ...
--     • Cannot derive Semigroup instance for AB due to sum type
--     • When deriving the instance for (Semigroup AB)
instance (AssertNoSum Semigroup a, Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
  (<>) = gmappend

-- | This uses the 'Semigroup' instance of the wrapped type 'a' to define 'mappend'.
-- The purpose of this instance is to derive 'mempty', while remaining consistent
-- with possibly custom 'Semigroup' instances.
instance (AssertNoSum Semigroup a, Semigroup a, Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
  mempty = gmempty
  mappend (Generically x) (Generically y) = Generically (x <> y)

instance (Generic a, GEnum StandardEnum (Rep a)) => Enum (Generically a) where
  toEnum = gtoEnum
  fromEnum = gfromEnum
  enumFrom = genumFrom
  enumFromThen = genumFromThen
  enumFromTo = genumFromTo
  enumFromThenTo = genumFromThenTo

instance (Generic a, GBounded (Rep a)) => Bounded (Generically a) where
  minBound = gminBound
  maxBound = gmaxBound

-- | Type with 'Enum' instance derived via 'Generic' with 'FiniteEnum' option.
newtype FiniteEnumeration a = FiniteEnumeration { unFiniteEnumeration :: a }

instance Generic a => Generic (FiniteEnumeration a) where
  type Rep (FiniteEnumeration a) = Rep a
  to = FiniteEnumeration . to
  from = from . unFiniteEnumeration

instance (Generic a, GEnum FiniteEnum (Rep a)) => Enum (FiniteEnumeration a) where
  toEnum = gtoFiniteEnum
  fromEnum = gfromFiniteEnum
  enumFrom = gfiniteEnumFrom
  enumFromThen = gfiniteEnumFromThen
  enumFromTo = gfiniteEnumFromTo
  enumFromThenTo = gfiniteEnumFromThenTo

-- | Type with instances derived via 'Generic1'.
newtype Generically1 f a = Generically1 { unGenerically1 :: f a }

instance Generic (f a) => Generic (Generically1 f a) where
  type Rep (Generically1 f a) = Rep (f a)
  to = Generically1 . to
  from = from . unGenerically1

instance Generic1 f => Generic1 (Generically1 f) where
  type Rep1 (Generically1 f) = Rep1 f
  to1 = Generically1 . to1
  from1 = from1 . unGenerically1

instance (Generic1 f, Eq1 (Rep1 f)) => Eq1 (Generically1 f) where
  liftEq = gliftEq

instance (Generic1 f, Eq1 (Rep1 f), Eq a) => Eq (Generically1 f a) where
  (==) = eq1

instance (Generic1 f, Ord1 (Rep1 f)) => Ord1 (Generically1 f) where
  liftCompare = gliftCompare

instance (Generic1 f, Ord1 (Rep1 f), Ord a) => Ord (Generically1 f a) where
  compare = compare1

instance (Generic1 f, GShow1 (Rep1 f)) => Show1 (Generically1 f) where
  liftShowsPrec = gliftShowsPrec

instance (Generic1 f, GShow1 (Rep1 f), Show a) => Show (Generically1 f a) where
  showsPrec = showsPrec1

instance (Generic1 f, Functor (Rep1 f)) => Functor (Generically1 f) where
  fmap = gfmap
  (<$) = gconstmap

instance (Generic1 f, Applicative (Rep1 f)) => Applicative (Generically1 f) where
  pure = gpure
  (<*>) = gap
#if MIN_VERSION_base(4,10,0)
  liftA2 = gliftA2
#endif

instance (Generic1 f, Alternative (Rep1 f)) => Alternative (Generically1 f) where
  empty = gempty
  (<|>) = galt

instance (Generic1 f, Foldable (Rep1 f)) => Foldable (Generically1 f) where
  foldMap = gfoldMap
  foldr = gfoldr

instance (Generic1 f, Traversable (Rep1 f)) => Traversable (Generically1 f) where
  traverse = gtraverse
  sequenceA = gsequenceA
