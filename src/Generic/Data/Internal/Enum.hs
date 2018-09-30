{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Internal.Enum where

import GHC.Generics

-- | Generic 'toEnum' generated with the 'StandardEnum' option.
--
-- @
-- instance 'Enum' MyType where
--   'toEnum' = 'gtoEnum'
--   'fromEnum' = 'gfromEnum'
-- @
gtoEnum :: forall a. (Generic a, GEnum StandardEnum (Rep a)) => Int -> a
gtoEnum n
  | 0 <= n && n < card = to (gToEnum @StandardEnum n)
  | otherwise = error $
      "gtoEnum: out of bounds, index " ++ show n ++ ", card " ++ show card
  where
    card = gCardinality @StandardEnum @(Rep a)

-- | Generic 'fromEnum' generated with the 'StandardEnum' option.
--
-- See also 'gtoEnum'.
gfromEnum :: (Generic a, GEnum StandardEnum (Rep a)) => a -> Int
gfromEnum = gFromEnum @StandardEnum . from

-- | Generic 'toEnum' generated with the 'SmallEnum' option.
--
-- @
-- instance 'Enum' MyType where
--   'toEnum' = 'gtoSmallEnum'
--   'fromEnum' = 'gfromSmallEnum'
-- @
gtoSmallEnum :: forall a. (Generic a, GEnum SmallEnum (Rep a)) => Int -> a
gtoSmallEnum n
  | 0 <= n && n < card = to (gToEnum @SmallEnum n)
  | otherwise = error $
      "gtoEnum: out of bounds, index " ++ show n ++ ", card " ++ show card
  where
    card = gCardinality @SmallEnum @(Rep a)

-- | Generic 'fromEnum' generated with the 'SmallEnum' option.
--
-- See also 'gtoSmallEnum'.
gfromSmallEnum :: (Generic a, GEnum SmallEnum (Rep a)) => a -> Int
gfromSmallEnum = gFromEnum @SmallEnum . from

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
-- have arbitrary many arguments. Each argument type/field must be an instance 
-- of both 'Enum' and 'Bounded'. Two restrictions require the user's caution:
--
-- * The instances of the field types need to be valid. Particularly 'Int' is an 
-- unfit field type, because the enumeration of the negative values starts 
-- before 0. 
--
-- * The generic type must not exceed the enumeration limit, hence the name 
-- SmallEnum. As 'Enum' converts from and to 'Int', at most @(maxBound :: Int)@ + 1
-- many values can be enumerated. This restriction makes 'Word' an invalid field 
-- type. Notably it is insufficient for each individual field types to stay
-- below the limit.
--
-- These restrictions are unlikely to apply if only Algebraic Data Types (ADTs)
-- are used as field types.
--
-- A 'GEnum' instance generically derived with this option will respect the 
-- generic 'Ord' instance. Implied by this, the values from different 
-- constructors are enumerated sequentially. They are not interleaved.
data SmallEnum

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

instance (GEnum SmallEnum f, GEnum SmallEnum g) => GEnum SmallEnum (f :*: g) where
  gCardinality = gCardinality @SmallEnum @f * gCardinality @SmallEnum @g
  gFromEnum (x :*: y) = gFromEnum @SmallEnum x * cardG + gFromEnum @SmallEnum y
    where
      cardG = gCardinality @SmallEnum @g
  gToEnum n = gToEnum @SmallEnum x :*: gToEnum @SmallEnum y
    where
      (x, y) = n `quotRem` cardG
      cardG = gCardinality @SmallEnum @g
  
instance GEnum opts U1 where
  gCardinality = 1
  gFromEnum U1 = 0
  gToEnum _ = U1

instance (Bounded c, Enum c) => GEnum SmallEnum (K1 i c) where
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
