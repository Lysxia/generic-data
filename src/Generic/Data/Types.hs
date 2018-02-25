-- | Utilities to derive and transform generic types.

{-# LANGUAGE TypeOperators #-}

module Generic.Data.Types
  ( Data(..)
  , Map

    -- * Defunctionalization
  , TyFun
  , type (~>)
  , type (@@)
  , Id
  , TyCon
  , Const
  ) where

import Generic.Data.Internal.Data
import Generic.Data.Internal.Defun
import Generic.Data.Internal.Functions
