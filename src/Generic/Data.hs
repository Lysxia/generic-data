-- | Generic combinators to derive type class instances.
--
-- == Orphans
--
-- The 'Data.Generic.Orphans' module should be imported to derive the following
-- classes using this library:
--
-- - 'Eq1' and 'Ord1'
-- - 'Semigroup' and 'Monoid' on GHC 8.4 or older (base <= 4.11)
--
-- == __Minor discrepancies__
--
-- Here are documented some corner cases of deriving, both by GHC and
-- generic-data. They are all minor and unlikely to cause problems in
-- practice.
--
-- === Empty types
--
-- - Some of the derived methods are lazy, which might result in errors
--   being silenced, though unlikely.
-- - The only generic-data implementation which differs from GHC stock
--   instances is 'gfoldMap'.
--
-- +------------------+-----------+--------------+-----------------------+
-- | Class method     | GHC stock | generic-data | Comment               |
-- +------------------+-----------+--------------+-----------------------+
-- | @('==')@         | lazy      | lazy         | 'True'                |
-- +------------------+-----------+--------------+-----------------------+
-- | 'compare'        | lazy      | lazy         | 'EQ'                  |
-- +------------------+-----------+--------------+-----------------------+
-- | 'fmap'           | strict    | strict       | must be bottom anyway |
-- +------------------+-----------+--------------+-----------------------+
-- | 'foldMap'        | lazy      | strict       | 'mempty' if lazy      |
-- +------------------+-----------+--------------+-----------------------+
-- | 'foldr'          | lazy      | lazy         | returns accumulator   |
-- +------------------+-----------+--------------+-----------------------+
-- | 'traverse'       | strict    | strict       |                       |
-- +------------------+-----------+--------------+-----------------------+
-- | 'sequenceA'      | strict    | strict       |                       |
-- +------------------+-----------+--------------+-----------------------+
--
-- === Single-constructor single-field types
--
-- @data@ types with one constructor and one field are extremely rare.
-- @newtype@ is almost always more appropriate (for which there is no issue).
--
-- That said, for @data@ types both strict and lazy, all generic-data
-- implementations are lazy (they don't even force the constructor),
-- whereas GHC stock implementations, when they exist, are strict.
--
-- === Functor composition
--
-- Fields of functors involving the composition of two or more functors
-- @f (g (h a))@ result in some overhead using "GHC.Generics.Generic1".
--
-- This is due to a particular encoding choice of @GHC.Generics@, where
-- composition are nested to the right instead of to the left. @f (g (h _))@ is
-- represented by the functor @f 'GHC.Generics.:.:' (g 'GHC.Generics.:.:' 'GHC.Generics.Rec1' h)@, so one must use
-- 'fmap' on @f@ to convert that back to @f (g (h _))@. A better choice would
-- have been to encode it as @('GHC.Generics.Rec1' f 'GHC.Generics.:.:' g) 'GHC.Generics.:.:' h@, because that is
-- coercible back to @f (g (h _))@.

module Generic.Data
  ( -- * Newtypes for Deriving Via
    Generically(..)
  , GenericProduct(..)
  , FiniteEnumeration(..)
  , Generically1(..)

    -- * Regular classes

    -- | Default implementations for classes indexed by types
    -- (kind @Type@).

    -- ** 'Semigroup'
  , gmappend

    -- ** 'Monoid'
  , gmempty
  , gmappend'

    -- ** 'Eq'
    -- | Can also be derived by GHC as part of the standard.
  , geq

    -- ** 'Ord'
    -- | Can also be derived by GHC as part of the standard.
  , gcompare

    -- ** 'Read'
    -- | Can also be derived by GHC as part of the standard.
  , greadPrec
  , GRead0

    -- ** 'Show'
    -- | Can also be derived by GHC as part of the standard.
  , gshowsPrec
  , GShow0

    -- ** 'Enum'
  , GEnum()
    -- *** 'StandardEnum' option
    -- | Can also be derived by GHC as part of the standard.
  , StandardEnum()
  , gtoEnum
  , gfromEnum
  , genumFrom
  , genumFromThen
  , genumFromTo
  , genumFromThenTo
    -- *** 'FiniteEnum' option
  , FiniteEnum()
  , gtoFiniteEnum
  , gfromFiniteEnum
  , gfiniteEnumFrom
  , gfiniteEnumFromThen
  , gfiniteEnumFromTo
  , gfiniteEnumFromThenTo

    -- ** 'Bounded'
    -- | Can also be derived by GHC as part of the standard.
  , gminBound
  , gmaxBound
  , GBounded()

    -- ** 'Ix'
    -- | Can also be derived by GHC as part of the standard.
  , grange
  , gindex
  , ginRange
  , GIx()
  , gunsafeIndex

    -- * Higher-kinded classes

    -- | Default implementations for classes indexed by type constructors
    -- (kind @Type -> Type@).

    -- ** 'Functor'
    -- | Can also be derived by GHC (@DeriveFunctor@ extension).
  , gfmap
  , gconstmap

    -- ** 'Foldable'
    -- | Can also be derived by GHC (@DeriveFoldable@ extension).
  , gfoldMap
  , gfoldr
  , GFoldable

    -- ** 'Traversable'
    -- | Can also be derived by GHC (@DeriveTraversable@ extension).
  , gtraverse
  , gsequenceA
  , GTraversable

    -- ** 'Applicative'
  , gpure
  , gap
  , gliftA2

    -- ** 'Alternative'
  , gempty
  , galt

    -- ** 'Eq1'
  , gliftEq

    -- ** 'Ord1'
  , gliftCompare

    -- ** 'Read1'
  , gliftReadPrec
  , GRead1

    -- ** 'Show1'
  , gliftShowsPrec
  , GShow1

    -- * Fields wrappers for deriving
  , Id1(..)
  , Opaque(..)
  , Opaque1(..)

    -- * Newtype
    -- | Generic pack/unpack.
  , Newtype
  , Old
  , pack
  , unpack

    -- * Generic coercions
  , gcoerce
  , gcoerceBinop

    -- * Accessing metadata

    -- | Using @TypeApplications@.

    -- ** Datatype
  , gdatatypeName
  , gmoduleName
  , gpackageName
  , gisNewtype
  , GDatatype

    -- ** Constructor
  , gconName
  , gconFixity
  , gconIsRecord
  , gconNum
  , gconIndex
  , Constructors
  , GConstructors

    -- *** Constructor tags
  , ConId()
  , conId
  , conIdToInt
  , conIdToString
  , conIdEnum
  , conIdNamed
  , ConIdNamed

  , conIdMin
  , conIdMax
  , NonEmptyType
  , IsEmptyType

  -- ** Using type families
  , MetaOf
  , MetaDataName
  , MetaDataModule
  , MetaDataPackage
  , MetaDataNewtype
  , MetaConsName
  , MetaConsFixity
  , MetaConsRecord
  , MetaSelNameM
  , MetaSelName
  , MetaSelUnpack
  , MetaSelSourceStrictness
  , MetaSelStrictness

    -- * The @Generic@ class

    -- | Reexported from "GHC.Generics".
  , Generic()
  , Generic1()
  ) where

import Generic.Data.Internal.Prelude hiding (gfoldMap, gtraverse, gsequenceA)
import Generic.Data.Internal.Enum
import Generic.Data.Internal.Generically
import Generic.Data.Internal.Meta
import Generic.Data.Internal.Read
import Generic.Data.Internal.Show
import Generic.Data.Internal.Traversable
import Generic.Data.Internal.Newtype
import Generic.Data.Internal.Resolvers
import Generic.Data.Internal.Utils

import GHC.Generics (Generic, Generic1)
