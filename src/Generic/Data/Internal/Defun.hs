{- | Defunctionalization

See https://hackage.haskell.org/package/singletons-2.4.1/docs/src/Data-Singletons-Internal.html#TyFun

A copy of the defunctionalization implementation in the singletons package, to
not pull in too heavy dependencies.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Internal.Defun where

import Data.Kind

type family   If (b :: Bool) (x :: k) (y :: k) :: k
type instance If 'True   x _y = x
type instance If 'False _x  y = y

class IsBool (b :: Bool) where
  _If :: ((b ~ 'True) => r) -> ((b ~ 'False) => r) -> r

instance IsBool 'True  where _If a _ = a
instance IsBool 'False where _If _ b = b

data TyExp_ :: k -> Type
type TyExp a = TyExp_ a -> Type

type family Eval (e :: TyExp_ a -> Type) :: a

data TyFun :: Type -> Type -> Type

-- | Kind of function symbols
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family (f :: TyFun k1 k2 -> Type) @@ (x :: k1) :: k2
infixl 9 @@

-- | Type constructor function symbol
data TyCon :: (k1 -> k2) -> TyFun k1 k2 -> Type
type instance TyCon f @@ x = f x

-- | Identity function symbol
data Id :: TyFun k1 k2 -> Type
type instance Id @@ x = x

-- | Constant function symbol
data Const :: k2 -> TyFun k1 k2 -> Type
type instance Const t @@ x = t
