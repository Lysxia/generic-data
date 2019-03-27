{-# LANGUAGE ExplicitNamespaces #-}

-- | Simple operations on generic representations, that only change the
-- type-level metadata used by certain generic functions.
--
-- More complex ones can be found in
-- <https://hackage.haskell.org/package/generic-data-surgery generic-data-surgery>
-- but also, perhaps surprisingly,
-- in <https://hackage.haskell.org/package/generic-lens generic-lens>
-- (read more about this just below) and
-- <https://hackage.haskell.org/package/one-liner one-liner>.

module Generic.Data.Microsurgery
  ( -- * Surgeries with generic-lens

    -- $lens-surgery

    -- * Deriving via

    Surgery
  , Surgery'(..)
  , GSurgery
  , Generically(..)

    -- * Synthetic types

  , Data
  , toData
  , fromData
  , onData

    -- * Microsurgeries
    --
    -- | Each microsurgery consists of a type family @F@ to modify metadata in
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

  , Derecordify()
  , derecordify
  , underecordify

    -- ** Type aging ("denewtypify")

  , Typeage()
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

  , RenameFields()
  , renameFields
  , unrenameFields

  , RenameConstrs()
  , renameConstrs
  , unrenameConstrs

    -- *** Renaming functions

  , type (@@)
  , SId
  , SError
  , SConst
  , SRename

    -- ** Wrap every field in a type constructor

    -- | Give every field a type @f FieldType@ (where @f@ is a parameter), to
    -- obtain a family of types with a shared structure. This
    -- \"higher-kindification\" technique is presented in the following
    -- blogposts:
    --
    -- - https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html
    -- - https://reasonablypolymorphic.com/blog/higher-kinded-data/
    --
    -- See also the file @test/one-liner-surgery.hs@ in this package for an
    -- example of using one-liner and generic-lens with a synthetic type
    -- constructed with 'DOnFields'.

  , OnFields()
  , DOnFields

  ) where

import Generic.Data.Internal.Data
import Generic.Data.Internal.Generically
import Generic.Data.Internal.Microsurgery

-- $lens-surgery
-- One common and simple situation is to modify the type of some fields,
-- for example wrapping them in a newtype.
--
-- We can leverage the @generic-lens@ library, with the two functions below.
--
-- @
-- -- Lens to a field named @fd@ in a Generic record.
-- field_ :: HasField_ fd s t a b => Lens s t a b  -- from generic-lens
--
-- -- Update a value through a lens (ASetter is a specialization of Lens).
-- over :: ASetter s t a b -> (a -> b) -> s -> t   -- from lens or microlens
-- @
--
-- For example, here is a record type:
--
-- @
-- data R = R { myField :: Int } deriving 'GHC.Generics.Generic'
-- @
--
-- The function @over (field_ \@\"myField\") 'Generic.Data.Opaque'@
-- applies the newtype constructor 'Generic.Data.Opaque' to the field
-- @\"myField\"@, but this actually doesn't typecheck as-is. With a bit of help
-- from this module, we can wrap that function as follows:
--
-- @
-- 'onData' (over (field_ \@\"myField\") 'Generic.Data.Opaque') . 'toData'
--   :: R -> 'Data' _ _   -- type arguments hidden
-- @
--
-- The result has a type @'Data' _ _@, that from the point of view of "GHC.Generics"
-- looks just like @R@ but with the field @\"myField\"@ wrapped in
-- 'Generic.Data.Opaque', as if we had defined:
--
-- @
-- data R = R { myField :: 'Generic.Data.Opaque' Int } deriving 'GHC.Generics.Generic'
-- @
--
-- ==== Example usage
--
-- We derive an instance of 'Show' that hides the @\"myField\"@ field,
-- whatever its type.
--
-- @
-- instance 'Show' R where
--   'showsPrec' n = 'Generic.Data.gshowsPrec' n
--     . 'onData' (over (field_ \@\"myField\") 'Generic.Data.Opaque')
--     . 'toData'
--
-- 'show' (R 3) = \"R {myField = _}\"
-- @
