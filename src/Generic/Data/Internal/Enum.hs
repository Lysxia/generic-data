{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Internal.Enum where

import GHC.Generics
import Data.Proxy

-- | Generic 'toEnum'.
--
-- @
-- instance 'Enum' MyType where
--   'toEnum' = 'gtoEnum'
--   'fromEnum' = 'gfromEnum'
-- @
gtoEnum :: forall a. (Generic a, GEnum (Rep a)) => Int -> a
gtoEnum n
  | 0 <= n && n < card = to (gToEnum n)
  | otherwise = error $
      "gtoEnum: out of bounds, index " ++ show n ++ ", card " ++ show card
  where
    card = gCardinality (Proxy :: Proxy (Rep a))

-- | Generic 'fromEnum'.
--
-- See also 'gtoEnum'.
gfromEnum :: (Generic a, GEnum (Rep a)) => a -> Int
gfromEnum = gFromEnum . from

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
class GEnum f where
  gCardinality :: proxy f -> Int
  gFromEnum :: f p -> Int
  gToEnum :: Int -> f p

instance GEnum f => GEnum (M1 i c f) where
  gCardinality _ = gCardinality (Proxy :: Proxy f)
  gFromEnum = gFromEnum . unM1
  gToEnum = M1 . gToEnum

instance (GEnum f, GEnum g) => GEnum (f :+: g) where
  gCardinality _ = gCardinality (Proxy :: Proxy f) + gCardinality (Proxy :: Proxy g)
  gFromEnum (L1 x) = gFromEnum x
  gFromEnum (R1 y) = cardF + gFromEnum y
    where
      cardF = gCardinality (Proxy :: Proxy f)
  gToEnum n
    | n < cardF = L1 (gToEnum n)
    | otherwise = R1 (gToEnum (n - cardF))
    where
      cardF = gCardinality (Proxy :: Proxy f)

instance GEnum U1 where
  gCardinality _ = 1
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
