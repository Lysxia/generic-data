{-# LANGUAGE ExplicitNamespaces #-}

-- | Simple operations on generic representations:
-- modify 'GHC.Generics.Generic' instances to tweak the behavior of generic
-- implementations as if you had declared a slightly different type.
--
-- This module provides the following microsurgeries:
--
-- - 'RenameFields': rename the fields of a record type.
-- - 'RenameConstrs': rename the constructors.
-- - 'OnFields': apply a type constructor @f :: Type -> Type@ to every field.
-- - 'CopyRep': use the generic representation of another type of the same shape.
-- - 'Typeage': treat a @newtype@ as a @data@ type.
-- - 'Derecordify': treat a type as if it weren't a record.
--
-- More complex surgeries can be found in
-- <https://hackage.haskell.org/package/generic-data-surgery generic-data-surgery>
-- but also, perhaps surprisingly,
-- in <https://hackage.haskell.org/package/generic-lens generic-lens>
-- (read more about this just below) and
-- <https://hackage.haskell.org/package/one-liner one-liner>.
--
-- Surgeries can be used:
--
-- - to derive type class instances with the @DerivingVia@ extension,
--   using the 'Surgery' or 'ProductSurgery' type synonyms
--   (for classes with instances for 'Generically' or 'GenericProduct');
-- - with the 'Data' \"synthetic type\" for more involved transformations,
--   for example using lenses in the next section.

module Generic.Data.Microsurgery
  ( -- * Surgeries with generic-lens

    -- $lens-surgery

    -- * Deriving via

    Surgery
  , ProductSurgery
  , Surgeries
  , ProductSurgeries
  , Surgery'(..)
  , GSurgery
  , Generically(..)
  , GenericProduct(..)

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
    --
    -- === Example
    --
    -- Derive 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' for
    -- a product of 'Prelude.Num' types:
    --
    -- @
    -- {-\# LANGUAGE DeriveGeneric, DerivingVia \#-}
    -- import "Data.Monoid" ('Data.Monoid.Sum'(..))  -- Constructors must be in scope
    -- import "GHC.Generics" ('GHC.Generics.Generic')
    -- import "Generic.Data.Microsurgery"
    --   ( 'ProductSurgery'
    --   , 'OnFields'
    --   , 'GenericProduct'(..)  -- Constructors must be in scope
    --   , 'Surgery''(..)        --
    --   )
    --
    -- data TwoCounters = MkTwoCounters { c1 :: Int, c2 :: Int }
    --   deriving 'GHC.Generics.Generic'
    --   deriving ('Data.Semigroup.Semigroup', 'Data.Monoid.Monoid')
    --     via ('ProductSurgery' ('OnFields' 'Data.Monoid.Sum') TwoCounters)  -- Surgery here
    -- @

  , OnFields()
  , DOnFields

  , OnField()
  , type (%~)

  , Cat()
  , DCat()

    -- ** Substitute a generic representation from another type

    -- |
    -- === Example
    --
    -- Derive 'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' for
    -- a product of 'Prelude.Num' types, but using 'Data.Monoid.Sum' for one
    -- field and 'Data.Monoid.Product' for the other.
    -- In other words, we use the fact that @Polar a@ below is isomorphic to
    -- the monoid @('Data.Monoid.Product' a, 'Data.Monoid.Sum' a)@.
    --
    -- @
    -- {-\# LANGUAGE DeriveGeneric, DerivingVia \#-}
    -- import "Data.Monoid" ('Data.Monoid.Sum'(..), 'Data.Monoid.Product'(..))  -- Constructors must be in scope
    -- import "GHC.Generics" ('GHC.Generics.Generic')
    -- import "Generic.Data.Microsurgery"
    --   ( 'ProductSurgery'
    --   , 'CopyRep'
    --   , 'GenericProduct'(..)  -- Constructors must be in scope
    --   , 'Surgery''(..)        --
    --   )
    --
    -- data Polar a = Exp { modulus :: a, argument :: a }
    --   deriving 'GHC.Generics.Generic'
    --   deriving ('Data.Semigroup.Semigroup', 'Data.Monoid.Monoid')
    --     via ('ProductSurgery' ('CopyRep' ('Data.Monoid.Product' a, 'Data.Monoid.Sum' a)) (Polar a))  -- Surgery here
    -- @
    --
    -- That is the polar representation of a complex number:
    --
    -- > z = modulus * exp(i * argument)
    --
    -- The product of complex numbers defines a monoid isomorphic to
    -- the monoid product @(Product Double, Sum Double)@
    -- (multiply the moduli, add the arguments).
    --
    -- @
    -- z1 'Data.Semigroup.<>' z2
    --  = z1 'Prelude.*' z2
    --  = Exp (modulus z1 'Prelude.*' modulus z2) (argument z1 'Prelude.+' argument z2)
    --
    -- 'Data.Monoid.mempty' = 1 = Exp 1 0
    -- @

  , CopyRep
  , copyRep
  , uncopyRep

    -- ** Type aging ("denewtypify")

  , Typeage()
  , typeage
  , untypeage

    -- ** Derecordify

  , Derecordify()
  , derecordify
  , underecordify

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
