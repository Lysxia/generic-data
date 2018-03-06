{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Generic.Data.Internal.Resolvers where

import Data.Bifunctor (first)
import Data.Functor.Classes
import Data.Function (on)
import Text.Read (Read(..))

import Generic.Data.Internal.Compat(readPrec1)

-- | A newtype whose instances for simple classes ('Eq', 'Ord', 'Read', 'Show')
-- use higher-kinded class instances for @f@.
newtype Id1 f a = Id1 { unId1 :: f a }
  deriving (Eq1, Ord1, Read1, Show1)

instance (Eq1 f, Eq a) => Eq (Id1 f a) where
  (==) = eq1 `on` unId1

instance (Ord1 f, Ord a) => Ord (Id1 f a) where
  compare = compare1 `on` unId1

instance (Read1 f, Read a) => Read (Id1 f a) where
  readsPrec = (fmap . fmap . fmap . first) Id1 readsPrec1
  readPrec = fmap Id1 readPrec1

instance (Show1 f, Show a) => Show (Id1 f a) where
  showsPrec d = showsPrec1 d . unId1

-- | A newtype with trivial instances, that considers
-- every value equivalent to every other one,
-- and shows as just @"_"@.
newtype Opaque a = Opaque { unOpaque :: a }

instance Eq (Opaque a) where
  (==) _ _ = True

instance Ord (Opaque a) where
  compare _ _ = EQ

instance Show (Opaque a) where
  showsPrec _ _ = showString "_"

instance Eq1 Opaque where
  liftEq _ _ _ = True

instance Ord1 Opaque where
  liftCompare _ _ _ = EQ

instance Show1 Opaque where
  liftShowsPrec _ _ _ _ = showString "_"

-- | A higher-kinded version of 'Opaque'.
newtype Opaque1 f a = Opaque1 { unOpaque1 :: f a }

instance Eq (Opaque1 f a) where
  (==) _ _ = True

instance Ord (Opaque1 f a) where
  compare _ _ = EQ

instance Show (Opaque1 f a) where
  showsPrec _ _ = showString "_"

instance Eq1 (Opaque1 f) where
  liftEq _ _ _ = True

instance Ord1 (Opaque1 f) where
  liftCompare _ _ _ = EQ

instance Show1 (Opaque1 f) where
  liftShowsPrec _ _ _ _ = showString "_"
