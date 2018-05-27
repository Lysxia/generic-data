-- | Newtypes with instances implemented using generic combinators.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic.Data.Internal.Generically where

import Control.Applicative
import Data.Functor.Classes
import Data.Semigroup
import GHC.Generics

import Generic.Data.Internal.Prelude
import Generic.Data.Internal.Enum
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

instance (Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
  (<>) = gmappend

instance (Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
  mempty = gmempty
  mappend = gmappend'

instance (Generic a, GEnum (Rep a)) => Enum (Generically a) where
  fromEnum = gfromEnum
  toEnum = gtoEnum

instance (Generic a, GBounded (Rep a)) => Bounded (Generically a) where
  minBound = gminBound
  maxBound = gmaxBound

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
