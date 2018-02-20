{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Enum where

import GHC.Generics
import Data.Proxy

toEnumG :: forall a. (Generic a, GEnum (Rep a)) => Int -> a
toEnumG n
  | 0 <= n && n < card = to (gToEnum n)
  | otherwise = error $
      "toEnumG: out of bounds, index " ++ show n ++ ", card " ++ show card
  where
    card = gCardinality (Proxy :: Proxy (Rep a))

fromEnumG :: (Generic a, GEnum (Rep a)) => a -> Int
fromEnumG = gFromEnum . from

minBoundG, maxBoundG :: (Generic a, GEnum (Rep a)) => a
minBoundG = to gMinBound
maxBoundG = to gMaxBound

gMinBound, gMaxBound :: forall f p. GEnum f => f p
gMinBound = gToEnum 0
gMaxBound = gToEnum (gCardinality (Proxy :: Proxy f))

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
