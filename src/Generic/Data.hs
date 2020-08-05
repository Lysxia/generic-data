-- | Generic combinators to derive type class instances.
--
-- = Overview
--
-- /base/ classes that GHC can not derive instances for, as of version 8.2:
--
-- - 'Data.Semigroup.Semigroup', 'Monoid', 'Applicative',
--   'Control.Applicative.Alternative', 'Data.Functor.Classes.Eq1',
--   'Data.Functor.Classes.Ord1', 'Data.Functor.Classes.Show1'.
--
-- On /base/ < 4.12 (i.e., GHC < 8.6), import "Generic.Data.Orphans" to obtain
-- instances needed internally to derive those.
--
-- GHC can derive instances for other classes here, although there may be
-- types supported by one method but not the other or vice versa.
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
-- Fields of functors involving the composition of two or more
-- functors @f (g (h a))@ cannot be handled nicely using @GHC.Generics@.
-- Some overhead cannot be safely avoided.
--
-- This is due to a particular encoding choice of @GHC.Generics@, where
-- composition are nested to the right instead of to the left. @f (g (h _))@ is
-- represented by the functor @f ':.:' (g ':.:' 'Rec1' h)@. A better choice is to
-- encode it as @('Rec1' f ':.:' g) ':.:' h@, because that is coercible back to
-- @f (g (h _))@.

module Generic.Data
  ( -- * Regular classes

    -- ** 'Data.Semigroup.Semigroup'
    gmappend

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
    -- *** StandardEnum option
    -- | Can also be derived by GHC as part of the standard.
  , StandardEnum()
  , gtoEnum
  , gfromEnum
  , genumFrom
  , genumFromThen
  , genumFromTo
  , genumFromThenTo
    -- *** FiniteEnum option
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

    -- ** 'Control.Applicative.Alternative'
  , gempty
  , galt

    -- ** 'Data.Functor.Classes.Eq1'
  , gliftEq

    -- ** 'Data.Functor.Classes.Ord1'
  , gliftCompare

    -- ** 'Data.Functor.Classes.Read1'
  , gliftReadPrec
  , GRead1

    -- ** 'Data.Functor.Classes.Show1'
  , gliftShowsPrec
  , GShow1

    -- * Fields wrappers for deriving
  , Id1(..)
  , Opaque(..)
  , Opaque1(..)

    -- * Carriers of generic instances
  , Generically(..)
  , GenericProduct(..)
  , FiniteEnumeration(..)
  , Generically1(..)

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
