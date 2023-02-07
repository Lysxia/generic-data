{-# LANGUAGE
  CPP,
  FlexibleContexts,
  TypeFamilies,
  UndecidableInstances,
  UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}
#if __GLASGOW_HASKELL__ >= 904
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

-- | Newtypes with instances implemented using generic combinators.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Generically
  ( Generically(..)
  , Generically1(..)
  , FiniteEnumeration(..)
  , GenericProduct(..) ) where

import GHC.Generics
import Data.Functor.Classes
import Data.Ix
import Text.Read

import Generic.Data.Internal.Prelude hiding (gfoldMap, gtraverse, gsequenceA)
import Generic.Data.Internal.Enum
import Generic.Data.Internal.Error
import Generic.Data.Internal.Read
import Generic.Data.Internal.Show
import Generic.Data.Internal.Traversable (GFoldable, GTraversable, gfoldMap, gtraverse, gsequenceA)

#if __GLASGOW_HASKELL__ < 904
import Control.Applicative
import Data.Semigroup
#endif

-- $setup
-- >>> :set -XDerivingVia -XDeriveGeneric
-- >>> import GHC.Generics (Generic, Generic1)

#if __GLASGOW_HASKELL__ < 904
-- | Type with instances derived via 'Generic'.
--
-- === Examples
--
-- ==== __Deriving 'Eq', 'Ord', 'Show', 'Read'__
--
-- >>> :{
-- data T = C Int Bool
--   deriving Generic
--   deriving (Eq, Ord, Show, Read) via (Generically T)
-- :}
--
-- ==== __Deriving 'Semigroup', 'Monoid'__
--
-- The type must have only one constructor.
--
-- >>> import Data.Monoid (Sum)
-- >>> :{
-- data U = D [Int] (Sum Int)
--   deriving Generic
--   deriving (Semigroup, Monoid) via (Generically U)
-- :}
--
-- ==== __Deriving 'Enum', 'Bounded'__
--
-- The type must have only nullary constructors.
-- To lift that restriction, see 'FiniteEnumeration'.
--
-- >>> :{
-- data V = X | Y | Z
--   deriving Generic
--   deriving (Eq, Ord, Enum, Bounded) via (Generically V)
-- :}
newtype Generically a = Generically a

instance (AssertNoSum Semigroup a, Generic a, Semigroup (Rep a ())) => Semigroup (Generically a) where
  (<>) = gmappend

-- | This uses the 'Semigroup' instance of the wrapped type @a@ to define 'mappend'.
-- The purpose of this instance is to derive 'mempty', while remaining consistent
-- with possibly custom 'Semigroup' instances.
instance (AssertNoSum Semigroup a, Semigroup a, Generic a, Monoid (Rep a ())) => Monoid (Generically a) where
  mempty = gmempty
  mappend (Generically x) (Generically y) = Generically (x <> y)
#endif

-- | This is a hack to implicitly wrap/unwrap in the instances of 'Generically'.
instance Generic a => Generic (Generically a) where
  type Rep (Generically a) = Rep a
  to = Generically . to
  from (Generically x) = from x

instance (Generic a, Eq (Rep a ())) => Eq (Generically a) where
  (==) = geq

instance (Generic a, Ord (Rep a ())) => Ord (Generically a) where
  compare = gcompare

instance (Generic a, GRead0 (Rep a)) => Read (Generically a) where
  readPrec = greadPrec
  readListPrec = readListPrecDefault

instance (Generic a, GShow0 (Rep a)) => Show (Generically a) where
  showsPrec = gshowsPrec

instance (Generic a, GEnum StandardEnum (Rep a)) => Enum (Generically a) where
  toEnum = gtoEnum
  fromEnum = gfromEnum
  enumFrom = genumFrom
  enumFromThen = genumFromThen
  enumFromTo = genumFromTo
  enumFromThenTo = genumFromThenTo

instance (Generic a, Ord (Rep a ()), GIx (Rep a)) => Ix (Generically a) where
  range = grange
  index = gindex
  inRange = ginRange

instance (Generic a, GBounded (Rep a)) => Bounded (Generically a) where
  minBound = gminBound
  maxBound = gmaxBound


-- | Type with 'Enum' instance derived via 'Generic' with 'FiniteEnum' option.
-- This allows deriving 'Enum' for types whose constructors have fields.
--
-- Some caution is advised; see details in 'FiniteEnum'.
--
-- === __Example__
--
-- >>> :{
-- data Booool = Booool Bool Bool
--   deriving Generic
--   deriving (Enum, Bounded) via (FiniteEnumeration Booool)
-- :}
newtype FiniteEnumeration a = FiniteEnumeration a

instance Generic a => Generic (FiniteEnumeration a) where
  type Rep (FiniteEnumeration a) = Rep a
  to = FiniteEnumeration . to
  from (FiniteEnumeration x) = from x

instance (Generic a, GEnum FiniteEnum (Rep a)) => Enum (FiniteEnumeration a) where
  toEnum = gtoFiniteEnum
  fromEnum = gfromFiniteEnum
  enumFrom = gfiniteEnumFrom
  enumFromThen = gfiniteEnumFromThen
  enumFromTo = gfiniteEnumFromTo
  enumFromThenTo = gfiniteEnumFromThenTo

-- | The same instance as 'Generically', for convenience.
instance (Generic a, GBounded (Rep a)) => Bounded (FiniteEnumeration a) where
  minBound = gminBound
  maxBound = gmaxBound

#if __GLASGOW_HASKELL__ < 904
-- | Type with instances derived via 'Generic1'.
--
-- === Examples
--
-- ==== __Deriving 'Functor', 'Applicative', 'Alternative'__
--
-- 'Applicative' can be derived for types with only one
-- constructor, aka. products.
--
-- >>> :{
-- data F a = F1 a | F2 (Maybe a) | F3 [Either Bool a] (Int, a)
--   deriving Generic1
--   deriving Functor via (Generically1 F)
-- :}
--
-- >>> :{
-- data G a = G a (Maybe a) [a] (IO a)
--   deriving Generic1
--   deriving (Functor, Applicative) via (Generically1 G)
-- :}
--
-- >>> import Control.Applicative (Alternative)
-- >>> :{
-- data G' a = G' (Maybe a) [a]
--   deriving Generic1
--   deriving (Functor, Applicative, Alternative) via (Generically1 G')
-- :}
--
-- ==== __Deriving 'Foldable'__
--
-- >>> import Generic.Data.Orphans ()
-- >>> :{
-- data H a = H1 a | H2 (Maybe a)
--   deriving Generic1
--   deriving (Functor, Foldable) via (Generically1 H)
-- :}
--
-- Note: we can't use @DerivingVia@ for 'Traversable'.
-- One may implement 'Traversable' explicitly using 'gtraverse'.
--
-- ==== __Deriving 'Eq1', 'Ord1'__
--
-- >>> import Data.Functor.Classes (Eq1, Ord1)
-- >>> :{
-- data I a = I [a] (Maybe a)
--   deriving Generic1
--   deriving (Eq1, Ord1) via (Generically1 I)
-- :}
newtype Generically1 f a = Generically1 (f a)

instance (Generic1 f, Eq1 (Rep1 f)) => Eq1 (Generically1 f) where
  liftEq = gliftEq

instance (Generic1 f, Ord1 (Rep1 f)) => Ord1 (Generically1 f) where
  liftCompare = gliftCompare

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
#endif

-- | This is a hack to implicitly wrap/unwrap in the instances of 'Generically1'.
instance Generic (f a) => Generic (Generically1 f a) where
  type Rep (Generically1 f a) = Rep (f a)
  to = Generically1 . to
  from (Generically1 x) = from x

-- | This is a hack to implicitly wrap/unwrap in the instances of 'Generically1'.
instance Generic1 f => Generic1 (Generically1 f) where
  type Rep1 (Generically1 f) = Rep1 f
  to1 = Generically1 . to1
  from1 (Generically1 x) = from1 x

#if !MIN_VERSION_base(4,18,0)
instance (Generic1 f, Eq1 (Rep1 f), Eq a) => Eq (Generically1 f a) where
  (==) = eq1

instance (Generic1 f, Ord1 (Rep1 f), Ord a) => Ord (Generically1 f a) where
  compare = compare1
#endif

instance (Generic1 f, GRead1 (Rep1 f)) => Read1 (Generically1 f) where
#if MIN_VERSION_base(4,10,0)
  liftReadPrec = gliftReadPrec
  liftReadListPrec = liftReadListPrecDefault
#else
  liftReadsPrec rp rl = readPrec_to_S $
    gliftReadPrec (readS_to_Prec rp) (readS_to_Prec (const rl))
#endif

instance (Generic1 f, GRead1 (Rep1 f), Read a) => Read (Generically1 f a) where
#if MIN_VERSION_base(4,10,0)
  readPrec = readPrec1
  readListPrec = readListPrecDefault
#else
  readsPrec = readsPrec1
#endif

instance (Generic1 f, GShow1 (Rep1 f)) => Show1 (Generically1 f) where
  liftShowsPrec = gliftShowsPrec

instance (Generic1 f, GShow1 (Rep1 f), Show a) => Show (Generically1 f a) where
  showsPrec = showsPrec1

instance (Generic1 f, GFoldable (Rep1 f)) => Foldable (Generically1 f) where
  foldMap = gfoldMap
  foldr = gfoldr

instance (Generic1 f, Functor (Rep1 f), GFoldable (Rep1 f), GTraversable (Rep1 f))
  => Traversable (Generically1 f) where
  traverse = gtraverse
  sequenceA = gsequenceA

-- | Product type with generic instances of 'Semigroup' and 'Monoid'.
--
-- This is similar to 'Generic.Data.Generically' in most cases, but
-- 'GenericProduct' also works for types @T@ with deriving
-- @via 'GenericProduct' U@, where @U@ is a generic product type coercible to,
-- but distinct from @T@. In particular, @U@ may not have an instance of
-- 'Semigroup', which 'Generic.Data.Generically' requires.
--
-- === __Example__
--
-- >>> import Data.Monoid (Sum(..))
-- >>> data Point a = Point a a deriving Generic
-- >>> :{
--   newtype Vector a = Vector (Point a)
--     deriving (Semigroup, Monoid)
--       via GenericProduct (Point (Sum a))
-- :}
--
-- If it were @via 'Generic.Data.Generically' (Point (Sum a))@ instead, then
-- @Vector@'s 'mappend' (the 'Monoid' method) would be defined as @Point@'s
-- @('<>')@ (the 'Semigroup' method), which might not exist, or might not be
-- equivalent to @Vector@'s generic 'Semigroup' instance, which would be
-- unlawful.
newtype GenericProduct a = GenericProduct a

instance Generic a => Generic (GenericProduct a) where
  type Rep (GenericProduct a) = Rep a
  to = GenericProduct . to
  from (GenericProduct x) = from x

instance (AssertNoSum Semigroup a, Generic a, Semigroup (Rep a ())) => Semigroup (GenericProduct a) where
  (<>) = gmappend

instance (AssertNoSum Semigroup a, Generic a, Monoid (Rep a ())) => Monoid (GenericProduct a) where
  mempty = gmempty
  mappend = gmappend'
