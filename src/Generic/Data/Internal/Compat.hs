{-# LANGUAGE
    CPP,
    DataKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Shim for backwards compatibility.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Compat
  ( readPrec1
  , Div
  ) where

import Data.Functor.Classes
import GHC.TypeLits

#if !MIN_VERSION_base(4,10,0)
import Text.ParserCombinators.ReadPrec (ReadPrec, readS_to_Prec)
import Text.Read (Read(..))
#endif

#if !MIN_VERSION_base(4,10,0)
readPrec1 :: (Read1 f, Read a) => ReadPrec (f a)
readPrec1 = readS_to_Prec $ liftReadsPrec readsPrec readList
#endif

#if !MIN_VERSION_base(4,11,0)
type Div m n = Div' (CmpNat m n) m n

type family   Div' (ord :: Ordering) (m :: Nat) (n :: Nat) :: Nat
type instance Div' 'LT m n = 0
type instance Div' 'GT m n = 1 + Div (m-n) n
type instance Div' 'EQ m n = 1
#endif
