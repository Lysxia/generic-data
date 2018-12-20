{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    PolyKinds,
    TypeFamilies,
    TypeOperators #-}

module Generic.Data.Internal.Microsurgery where

import Data.Coerce (Coercible, coerce)
import GHC.Generics

import Generic.Data.Types

unsetIsRecord ::
  Coercible (UnsetIsRecord f p) (f p) =>
  -- Coercible is not symmetric!??
  Data f p -> Data (UnsetIsRecord f) p
unsetIsRecord = coerce

resetIsRecord ::
  Coercible (f p) (UnsetIsRecord f p) =>
  Data (UnsetIsRecord f) p -> Data f p
resetIsRecord = coerce

type family UnsetIsRecord (f :: k -> *) :: k -> *
type instance UnsetIsRecord (M1 D m f) = M1 D m (UnsetIsRecord f)
type instance UnsetIsRecord (f :+: g) = UnsetIsRecord f :+: UnsetIsRecord g
type instance UnsetIsRecord (M1 C ('MetaCons nm fx _isRecord) f) = M1 C ('MetaCons nm fx 'False) f
type instance UnsetIsRecord V1 = V1
