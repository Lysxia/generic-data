-- | Generic combinators to derive type class instances.
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

    -- ** 'Traversable'
    -- | Can also be derived by GHC (@DeriveTraversable@ extension).
  , gtraverse
  , gsequenceA

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

import Generic.Data.Internal.Prelude
import Generic.Data.Internal.Enum
import Generic.Data.Internal.Generically
import Generic.Data.Internal.Meta
import Generic.Data.Internal.Read
import Generic.Data.Internal.Show
import Generic.Data.Internal.Newtype
import Generic.Data.Internal.Resolvers
import Generic.Data.Internal.Utils
