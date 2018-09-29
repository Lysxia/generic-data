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

-- | Generic 'toEnum'.
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

-- | Generic 'fromEnum'.
--
-- See also 'gtoEnum'.
gfromEnum :: (Generic a, GEnum StandardEnum (Rep a)) => a -> Int
gfromEnum = gFromEnum @StandardEnum . from

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

instance GEnum opts U1 where
  gCardinality = 1
  gFromEnum U1 = 0
  gToEnum _ = U1

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
