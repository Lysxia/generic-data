{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Newtypes with special instances for deriving.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Resolvers where

import Data.Bifunctor (first)
import Data.Functor.Classes
import Data.Function (on)
import Text.Read (Read(..))

import Generic.Data.Internal.Compat(readPrec1)

-- | A newtype whose instances for simple classes ('Eq', 'Ord', 'Read', 'Show')
-- use higher-kinded class instances for @f@ (`Eq1`, `Ord1`, `Read1`, `Show1`).
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

-- | All equal.
instance Eq (Opaque a) where
  (==) _ _ = True

-- | All equal.
instance Ord (Opaque a) where
  compare _ _ = EQ

-- | Shown as @"_"@.
instance Show (Opaque a) where
  showsPrec _ _ = showString "_"

-- | All equal.
instance Eq1 Opaque where
  liftEq _ _ _ = True

-- | All equal.
instance Ord1 Opaque where
  liftCompare _ _ _ = EQ

-- | Shown as @"_"@.
instance Show1 Opaque where
  liftShowsPrec _ _ _ _ = showString "_"

-- | A higher-kinded version of 'Opaque'.
newtype Opaque1 f a = Opaque1 { unOpaque1 :: f a }

-- | All equal.
instance Eq (Opaque1 f a) where
  (==) _ _ = True

-- | All equal.
instance Ord (Opaque1 f a) where
  compare _ _ = EQ

-- | Shown as @"_"@.
instance Show (Opaque1 f a) where
  showsPrec _ _ = showString "_"

-- | All equal.
instance Eq1 (Opaque1 f) where
  liftEq _ _ _ = True

-- | All equal.
instance Ord1 (Opaque1 f) where
  liftCompare _ _ _ = EQ

-- | Shown as @"_"@.
instance Show1 (Opaque1 f) where
  liftShowsPrec _ _ _ _ = showString "_"
