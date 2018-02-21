{- | Defunctionalization

See https://hackage.haskell.org/package/singletons-2.4.1/docs/src/Data-Singletons-Internal.html#TyFun

A copy of the defunctionalization implementation in the singletons package, to
not pull in too heavy dependencies.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Generic.Data.Defun where

import Data.Kind

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
