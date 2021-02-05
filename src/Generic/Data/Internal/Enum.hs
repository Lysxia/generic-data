{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Generic deriving for 'Enum', 'Bounded' and 'Ix'.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Enum where

import GHC.Generics
import Data.Ix

-- | Generic 'toEnum' generated with the 'StandardEnum' option.
--
-- @
-- instance 'Enum' MyType where
--   'toEnum' = 'gtoEnum'
--   'fromEnum' = 'gfromEnum'
--   'enumFrom' = 'genumFrom'
--   'enumFromThen' = 'genumFromThen'
--   'enumFromTo' = 'genumFromTo'
--   'enumFromThenTo' = 'genumFromThenTo'
-- @
gtoEnum :: (Generic a, GEnum StandardEnum (Rep a)) => Int -> a
gtoEnum = gtoEnum' @StandardEnum "gtoEnum"

-- | Generic 'fromEnum' generated with the 'StandardEnum' option.
--
-- See also 'gtoEnum'.
gfromEnum :: (Generic a, GEnum StandardEnum (Rep a)) => a -> Int
gfromEnum = gfromEnum' @StandardEnum

-- | Generic 'enumFrom' generated with the 'StandardEnum' option.
--
-- See also 'gtoEnum'.
genumFrom :: (Generic a, GEnum StandardEnum (Rep a)) => a -> [a]
genumFrom = genumFrom' @StandardEnum

-- | Generic 'enumFromThen' generated with the 'StandardEnum' option.
--
-- See also 'gtoEnum'.
genumFromThen :: (Generic a, GEnum StandardEnum (Rep a)) => a -> a -> [a]
genumFromThen = genumFromThen' @StandardEnum

-- | Generic 'enumFromTo' generated with the 'StandardEnum' option.
--
-- See also 'gtoEnum'.
genumFromTo :: (Generic a, GEnum StandardEnum (Rep a)) => a -> a -> [a]
genumFromTo = genumFromTo' @StandardEnum

-- | Generic 'enumFromThenTo' generated with the 'StandardEnum' option.
--
-- See also 'gtoEnum'.
genumFromThenTo :: (Generic a, GEnum StandardEnum (Rep a)) => a -> a -> a -> [a]
genumFromThenTo = genumFromThenTo' @StandardEnum


-- | Generic 'toEnum' generated with the 'FiniteEnum' option.
--
-- @
-- instance 'Enum' MyType where
--   'toEnum' = 'gtoFiniteEnum'
--   'fromEnum' = 'gfromFiniteEnum'
--   'enumFrom' = 'gfiniteEnumFrom'
--   'enumFromThen' = 'gfiniteEnumFromThen'
--   'enumFromTo' = 'gfiniteEnumFromTo'
--   'enumFromThenTo' = 'gfiniteEnumFromThenTo'
-- @
gtoFiniteEnum :: (Generic a, GEnum FiniteEnum (Rep a)) => Int -> a
gtoFiniteEnum = gtoEnum' @FiniteEnum "gtoFiniteEnum"

-- | Generic 'fromEnum' generated with the 'FiniteEnum' option.
--
-- See also 'gtoFiniteEnum'.
gfromFiniteEnum :: (Generic a, GEnum FiniteEnum (Rep a)) => a -> Int
gfromFiniteEnum = gfromEnum' @FiniteEnum

-- | Generic 'enumFrom' generated with the 'FiniteEnum' option.
--
-- See also 'gtoFiniteEnum'.
gfiniteEnumFrom :: (Generic a, GEnum FiniteEnum (Rep a)) => a -> [a]
gfiniteEnumFrom = genumFrom' @FiniteEnum

-- | Generic 'enumFromThen' generated with the 'FiniteEnum' option.
--
-- See also 'gtoFiniteEnum'.
gfiniteEnumFromThen :: (Generic a, GEnum FiniteEnum (Rep a)) => a -> a -> [a]
gfiniteEnumFromThen = genumFromThen' @FiniteEnum

-- | Generic 'enumFromTo' generated with the 'FiniteEnum' option.
--
-- See also 'gtoFiniteEnum'.
gfiniteEnumFromTo :: (Generic a, GEnum FiniteEnum (Rep a)) => a -> a -> [a]
gfiniteEnumFromTo = genumFromTo' @FiniteEnum

-- | Generic 'enumFromThenTo' generated with the 'FiniteEnum' option.
--
-- See also 'gtoFiniteEnum'.
gfiniteEnumFromThenTo :: (Generic a, GEnum FiniteEnum (Rep a)) => a -> a -> a -> [a]
gfiniteEnumFromThenTo = genumFromThenTo' @FiniteEnum

-- | Unsafe generic 'toEnum'. Does not check whether the argument is within
-- valid bounds. Use 'gtoEnum' or 'gtoFiniteEnum' instead.
gtoEnumRaw' :: forall opts a. (Generic a, GEnum opts (Rep a)) => Int -> a
gtoEnumRaw' = to . gToEnum @opts

-- | Generic 'toEnum'. Use 'gfromEnum' or 'gfromFiniteEnum' instead.
gtoEnum' :: forall opts a. (Generic a, GEnum opts (Rep a)) => String -> Int -> a
gtoEnum' name n
  | 0 <= n && n < card = gtoEnumRaw' @opts n
  | otherwise = error $
      name ++ ": out of bounds, index " ++ show n ++ ", cardinality " ++ show card
  where
    card = gCardinality @opts @(Rep a)

-- | Generic 'fromEnum'. Use 'gfromEnum' or 'gfromFiniteEnum' instead.
gfromEnum' :: forall opts a. (Generic a, GEnum opts (Rep a)) => a -> Int
gfromEnum' = gFromEnum @opts . from

-- | > genumMin == gfromEnum gminBound
genumMin :: Int
genumMin = 0

-- | > genumMax == gfromEnum gmaxBound
genumMax :: forall opts a. (Generic a, GEnum opts (Rep a)) => Int
genumMax = gCardinality @opts @(Rep a) - 1

-- | Generic 'enumFrom'. Use 'genumFrom' or 'gfiniteEnumFrom' instead.
genumFrom' :: forall opts a. (Generic a, GEnum opts (Rep a)) => a -> [a]
genumFrom' x = map toE [ i_x .. genumMax @opts @a ]
  where
    toE = gtoEnumRaw' @opts
    i_x = gfromEnum'  @opts x

-- | Generic 'enumFromThen'. Use 'genumFromThen' or 'gfiniteEnumFromThen' instead.
genumFromThen' :: forall opts a. (Generic a, GEnum opts (Rep a)) => a -> a -> [a]
genumFromThen' x1 x2 = map toE [ i_x1, i_x2 .. bound ]
  where
    toE  = gtoEnumRaw' @opts
    i_x1 = gfromEnum'  @opts x1
    i_x2 = gfromEnum'  @opts x2
    bound | i_x1 >= i_x2 = genumMin
          | otherwise    = genumMax @opts @a

-- | Generic 'enumFromTo'. Use 'genumFromTo' or 'gfiniteEnumFromTo' instead.
genumFromTo' :: forall opts a. (Generic a, GEnum opts (Rep a)) => a -> a -> [a]
genumFromTo' x y = map toE [ i_x .. i_y ]
  where
    toE = gtoEnumRaw' @opts
    i_x = gfromEnum'  @opts x
    i_y = gfromEnum'  @opts y

-- | Generic 'enumFromThenTo'. Use 'genumFromThenTo' or 'gfiniteEnumFromThenTo' instead.
genumFromThenTo' :: forall opts a. (Generic a, GEnum opts (Rep a)) => a -> a -> a -> [a]
genumFromThenTo' x1 x2 y = map toE [ i_x1, i_x2 .. i_y ]
  where
    toE  = gtoEnumRaw' @opts
    i_x1 = gfromEnum'  @opts x1
    i_x2 = gfromEnum'  @opts x2
    i_y  = gfromEnum'  @opts y

-- | Generic 'minBound'.
--
-- @
-- instance 'Bounded' MyType where
--   'minBound' = 'gminBound'
--   'maxBound' = 'gmaxBound'
-- @
gminBound :: (Generic a, GBounded (Rep a)) => a
gminBound = to gMinBound

-- | Generic 'maxBound'.
--
-- See also 'gminBound'.
gmaxBound :: (Generic a, GBounded (Rep a)) => a
gmaxBound = to gMaxBound

-- | Generic 'range'.
--
-- @
-- import "Data.Ix"
-- instance 'Ix' MyType where
--   'range' = 'grange'
--   'index' = 'gindex'
--   'inRange' = 'ginRange'
-- @
grange :: (Generic a, GIx (Rep a)) => (a, a) -> [a]
grange (m, n) = map to $ gRange (from m, from n)

-- | Generic 'index'.
--
-- See also 'grange'.
gindex :: (Generic a, GIx (Rep a)) => (a, a) -> a -> Int
gindex b i
  | ginRange b i = gunsafeIndex b i
  | otherwise = errorWithoutStackTrace "gindex: out of bounds"

-- | Generic @unsafeIndex@.
--
-- === __Details__
--
-- The functions @unsafeIndex@ and @unsafeRangeSize@ belong to 'Ix' but are
-- internal to GHC and hence not exported from the module "Data.Ix". However they
-- are exported from the module @GHC.Arr@.
-- See 'grange' for how to define an instance of 'Ix' such that it does not
-- depend on the stability of GHCs internal API. Unfortunately this results in
-- additional (unnecessary) bound checks.
-- With the danger of having no stability guarantees for GHC's internal API one
-- can alternatively define an instance of 'Ix' as
--
-- @
-- import GHC.Arr
-- instance 'Ix' MyType where
--   'range' = 'grange'
--   unsafeIndex = 'gunsafeIndex'
--   'inRange' = 'ginRange'
-- @
gunsafeIndex :: (Generic a, GIx (Rep a)) => (a, a) -> a -> Int
gunsafeIndex (m, n) i = gUnsafeIndex (from m, from n) (from i)

-- | Generic 'inRange'.
--
-- See also 'grange'.
ginRange :: (Generic a, GIx (Rep a)) => (a, a) -> a -> Bool
ginRange (m, n) i = gInRange (from m, from n) (from i)

-- | Generic representation of 'Enum' types.
--
-- The @opts@ parameter is a type-level option to select different
-- implementations.
class GEnum opts f where
  gCardinality :: Int
  gFromEnum :: f p -> Int
  gToEnum :: Int -> f p

-- | Standard option for 'GEnum': derive 'Enum' for types with only nullary
-- constructors (the same restrictions as in the [Haskell 2010
-- report](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html#x18-18400011.2)).
data StandardEnum

-- | Extends the 'StandardEnum' option for 'GEnum' to allow all constructors to 
-- have arbitrary many fields. Each field type must be an instance of 
-- both 'Enum' and 'Bounded'. Avoid fields of types 'Int' and 'Word'.
--
-- === __Details__
--
-- Two restrictions require the user's attention:
--
-- * The 'Enum' instances of the field types need to start enumerating from 0. 
-- In particular, 'Int' is an unfit field type, because the enumeration of the
-- negative values starts before 0. 
--
-- * There can only be up to @'maxBound' :: 'Int'@ values (because the implementation
-- represents the cardinality explicitly as an 'Int'). This restriction makes
-- 'Word' an invalid field type as well. Notably, it is insufficient for each
-- individual field types to stay below this limit. Instead it applies to the
-- generic type as a whole.
--
-- Elements are numbered by 'toEnum', from @0@ up to @(cardinality - 1)@.
-- The resulting ordering matches the generic 'Ord' instance defined by
-- 'Generic.Data.gcompare'.
-- The values from different constructors are enumerated sequentially.
--
-- @
-- data Example = C0 Bool Bool | C1 Bool
--   deriving ('Eq', 'Ord', 'Show', 'Generic')
--
-- cardinality = 6  -- 2    * 2    + 2
--                  -- Bool * Bool | Bool
--
-- enumeration =
--     [ C0 False False
--     , C0 False  True
--     , C0  True False
--     , C0  True  True
--     , C1 False
--     , C1 True
--     ]
--
-- enumeration == map 'gtoFiniteEnum' [0 .. 5]
-- [0 .. 5] == map 'gfromFiniteEnum' enumeration
-- @
data FiniteEnum

instance GEnum opts f => GEnum opts (M1 i c f) where
  gCardinality = gCardinality @opts @f
  gFromEnum = gFromEnum @opts . unM1
  gToEnum = M1 . gToEnum @opts

instance (GEnum opts f, GEnum opts g) => GEnum opts (f :+: g) where
  gCardinality = gCardinality @opts @f + gCardinality @opts @g
  gFromEnum (L1 x) = gFromEnum @opts x
  gFromEnum (R1 y) = cardF + gFromEnum @opts y
    where
      cardF = gCardinality @opts @f
  gToEnum n
    | n < cardF = L1 (gToEnum @opts n)
    | otherwise = R1 (gToEnum @opts (n - cardF))
    where
      cardF = gCardinality @opts @f

instance (GEnum FiniteEnum f, GEnum FiniteEnum g) => GEnum FiniteEnum (f :*: g) where
  gCardinality = gCardinality @FiniteEnum @f * gCardinality @FiniteEnum @g
  gFromEnum (x :*: y) = gFromEnum @FiniteEnum x * cardG + gFromEnum @FiniteEnum y
    where
      cardG = gCardinality @FiniteEnum @g
  gToEnum n = gToEnum @FiniteEnum x :*: gToEnum @FiniteEnum y
    where
      (x, y) = n `quotRem` cardG
      cardG = gCardinality @FiniteEnum @g
  
instance GEnum opts U1 where
  gCardinality = 1
  gFromEnum U1 = 0
  gToEnum _ = U1

instance (Bounded c, Enum c) => GEnum FiniteEnum (K1 i c) where
  gCardinality = fromEnum (maxBound :: c) + 1
  gFromEnum = fromEnum . unK1
  gToEnum = K1 . toEnum

-- | Generic representation of 'Bounded' types.
class GBounded f where
  gMinBound :: f p
  gMaxBound :: f p

deriving instance GBounded f => GBounded (M1 i c f)

instance GBounded U1 where
  gMinBound = U1
  gMaxBound = U1

instance Bounded c => GBounded (K1 i c) where
  gMinBound = K1 minBound
  gMaxBound = K1 maxBound

instance (GBounded f, GBounded g) => GBounded (f :+: g) where
  gMinBound = L1 gMinBound
  gMaxBound = R1 gMaxBound

instance (GBounded f, GBounded g) => GBounded (f :*: g) where
  gMinBound = gMinBound :*: gMinBound
  gMaxBound = gMaxBound :*: gMaxBound

-- | Generic representation of 'Ix' types.
--
class GIx f where
  gRange :: (f p, f p) -> [f p]
  gUnsafeIndex :: (f p, f p) -> f p -> Int
  gInRange :: (f p, f p) -> f p -> Bool

instance GIx f => GIx (M1 i c f) where
  gRange (M1 m, M1 n) = map M1 $ gRange (m, n)
  gUnsafeIndex (M1 m, M1 n) (M1 i) = gUnsafeIndex (m, n) i
  gInRange (M1 m, M1 n) (M1 i) = gInRange (m, n) i

instance (GEnum StandardEnum f, GEnum StandardEnum g) => GIx (f :+: g) where
  gRange (x, y) = map toE [ i_x .. i_y ]
    where
      toE = gToEnum @StandardEnum
      i_x = gFromEnum @StandardEnum x
      i_y = gFromEnum @StandardEnum y
  gUnsafeIndex (m, _) i = fromIntegral (i_i - i_m)
    where
      i_m = gFromEnum @StandardEnum m
      i_i = gFromEnum @StandardEnum i
  gInRange (m, n) i = i_m <= i_i && i_i <= i_n
    where
      i_m = gFromEnum @StandardEnum m
      i_n = gFromEnum @StandardEnum n
      i_i = gFromEnum @StandardEnum i

instance (GIx f, GIx g) => GIx (f :*: g) where
  gRange (m1 :*: m2, n1 :*: n2) =
    [ i1 :*: i2 | i1 <- gRange (m1, n1), i2 <- gRange (m2, n2) ]
  gUnsafeIndex (m1 :*: m2, n1 :*: n2) (i1 :*: i2) = int1 * rangeSize2 + int2
    where
      int1 = gUnsafeIndex (m1, n1) i1
      int2 = gUnsafeIndex (m2, n2) i2
      rangeSize2 = gUnsafeIndex (m2, n2) n2 + 1
  gInRange (m1 :*: m2, n1 :*: n2) (i1 :*: i2) =
    gInRange (m1, n1) i1 && gInRange (m2, n2) i2
  
instance GIx U1 where
  gRange (U1, U1) = [U1]
  gUnsafeIndex (U1, U1) U1 = 0
  gInRange (U1, U1) U1 = True

instance (Ix c) => GIx (K1 i c) where
  gRange (K1 m, K1 n) = map K1 $ range (m, n)
  gUnsafeIndex (K1 m, K1 n) (K1 i) = index (m, n) i 
  gInRange (K1 m, K1 n) (K1 i) = inRange (m, n) i
