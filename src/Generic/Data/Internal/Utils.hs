{-# LANGUAGE
    BangPatterns,
    EmptyCase,
    FlexibleContexts,
    PolyKinds,
    Trustworthy #-}

-- | Utilities.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Utils where

import Data.Coerce
import GHC.Generics
import GHC.Lexeme (startsConSym, startsVarSym)

-- | Convert between types with representationally equivalent generic
-- representations.
gcoerce
  :: (Generic a, Generic b, Coercible (Rep a) (Rep b))
  => a -> b
gcoerce = to . coerce1 . from

-- | Compose 'gcoerce' with a binary operation.
gcoerceBinop
  :: (Generic a, Generic b, Coercible (Rep a) (Rep b))
  => (a -> a -> a) -> (b -> b -> b)
gcoerceBinop f x y = gcoerce (f (gcoerce x) (gcoerce y))

-- | Coerce while preserving the type index.
coerce' :: Coercible (f x) (g x) => f x -> g x
coerce' = coerce

coerce1 :: Coercible f g => f x -> g x
coerce1 = coerce

-- | Elimination of @V1@.
absurd1 :: V1 x -> a
absurd1 x = case x of {}

-- | A helper for better type inference.
from' :: Generic a => a -> Rep a ()
from' = from

-- | A helper for better type inference.
to' :: Generic a => Rep a () -> a
to' = to

-- | Lift binary combinators generically.
liftG2 :: Generic1 f => (Rep1 f a -> Rep1 f b -> Rep1 f c) -> f a -> f b -> f c
liftG2 = \(<?>) a b -> to1 (from1 a <?> from1 b)

-- | Returns 'True' if the argument is a symbolic data constructor name
-- (e.g., @(:+:)@). Returns 'False' otherwise.
isSymDataCon :: String -> Bool
isSymDataCon ""    = False
isSymDataCon (c:_) = startsConSym c

-- | Returns 'True' if the argument is a symbolic value name (e.g., @(+++)@).
-- Returns 'False' otherwise.
isSymVar :: String -> Bool
isSymVar ""    = False
isSymVar (c:_) = startsVarSym c
