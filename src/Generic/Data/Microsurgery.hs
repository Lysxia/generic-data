{-# LANGUAGE ExplicitNamespaces #-}

-- | Simple operations on generic representations.
--
-- More complex ones can be found in
-- <https://hackage.haskell.org/package/generic-data-surgery generic-data-surgery>.

module Generic.Data.Microsurgery
  ( -- * Synthetic types
    -- | Reexported from "Generic.Data.Types"

    Data
  , toData
  , fromData

    -- * Microsurgeries

    -- ** Derecordify

  , derecordify
  , underecordify
  , Derecordify

    -- ** Type aging ("denewtypify")

  , typeage
  , untypeage
  , Typeage

    -- ** Renaming of fields and constructors

  , renameFields
  , unrenameFields
  , RenameFields

  , renameConstrs
  , unrenameConstrs
  , RenameConstrs

    -- *** Renaming functions

  , type (@@)
  , SId
  , SError
  , SConst
  , SRename

  ) where

import Generic.Data.Internal.Data
import Generic.Data.Internal.Microsurgery
