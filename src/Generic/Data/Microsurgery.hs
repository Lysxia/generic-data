{-# LANGUAGE ExplicitNamespaces #-}

-- | Simple operations on generic representations, that only change the
-- type-level metadata.
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
    -- | Each microsurgery consists of a type family @F@ updating metadata in
    -- GHC Generic representations, and two mappings (that are just
    -- 'Data.Coerce.coerce'):
    --
    -- @
    --   f :: 'Data' ('GHC.Generics.Rep' a) p -> 'Data' (F ('GHC.Generics.Rep' a)) p
    -- unf :: 'Data' (F ('GHC.Generics.Rep' a)) p -> 'Data' ('GHC.Generics.Rep' a) p
    -- @
    --
    -- Use @f@ with 'toData' for generic functions that consume generic values,
    -- and @unf@ with 'fromData' for generic functions that produce generic
    -- values. Abstract example:
    --
    -- @
    -- genericSerialize . f . 'toData'
    -- 'fromData' . unf . genericDeserialize
    -- @

    -- ** Derecordify

  , Derecordify
  , derecordify
  , underecordify

    -- ** Type aging ("denewtypify")

  , Typeage
  , typeage
  , untypeage

    -- ** Renaming of fields and constructors
    -- | These surgeries require @DataKinds@ and @TypeApplications@.
    --
    -- ==== Examples
    --
    -- @
    -- {-# LANGUAGE
    --     DataKinds,
    --     TypeApplications #-}
    --
    -- -- Rename all fields to \"foo\"
    -- 'renameFields' \@('SConst' \"foo\")
    --
    -- -- Rename constructor \"Bar\" to \"Baz\", and leave all others the same
    -- 'renameConstrs' \@('SRename' '[ '(\"Bar\", \"Baz\") ] 'SId')
    -- @

  , RenameFields
  , renameFields
  , unrenameFields

  , RenameConstrs
  , renameConstrs
  , unrenameConstrs

    -- *** Renaming functions

  , type (@@)
  , SId
  , SError
  , SConst
  , SRename

  ) where

import Generic.Data.Internal.Data
import Generic.Data.Internal.Microsurgery
