{-# LANGUAGE
    BangPatterns,
    PolyKinds #-}

module Generic.Data.Internal.Utils where

import Data.Coerce
import GHC.Generics

-- | Coerce while preserving the type index.
coerce' :: Coercible (f x) (g x) => f x -> g x
coerce' = coerce

-- | Elimination of @V1@.
absurd1 :: V1 x -> a
absurd1 !_ = error "impossible"

-- | A helper for better type inference.
from' :: Generic a => a -> Rep a ()
from' = from

-- | A helper for better type inference.
to' :: Generic a => Rep a () -> a
to' = to

-- | Lift binary combinators generically.
liftG2 :: Generic1 f => (Rep1 f a -> Rep1 f b -> Rep1 f c) -> f a -> f b -> f c
liftG2 = \(<?>) a b -> to1 (from1 a <?> from1 b)
