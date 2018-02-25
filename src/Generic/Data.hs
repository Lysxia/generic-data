-- | Generic combinators to derive type class instances.

module Generic.Data
  ( -- * Deriving instances

    -- | Classes that GHC can not derive (excluding @GeneralizedNewtypeDeriving@):
    -- 'Data.Semigroup.Semigroup', 'Monoid', 'Applicative',
    -- 'Control.Applicative.Alternative', 'Data.Functor.Classes.Eq1',
    -- 'Data.Functor.Classes.Ord1', 'Data.Functor.Classes.Show1'.
    --
    -- On base < 4.12 (i.e., GHC < 8.8), you should import "Generic.Data.Orphans"
    -- to derive those.

    -- ** 'Data.Semigroup.Semigroup'
    gmappend

    -- ** 'Monoid'
  , gmempty
  , gmappend'

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

    -- ** 'Data.Functor.Classes.Show1'
  , gliftShowsPrec
  , GShow1

    -- * Other classes

    -- | GHC can already derive these.

    -- ** 'Eq'
  , geq

    -- ** 'Ord'
  , gcompare

    -- ** 'Show'
  , gshowsPrec
  , GShow0

    -- ** 'Enum'
  , gfromEnum
  , gtoEnum
  , GEnum()

    -- ** 'Bounded'
  , gminBound
  , gmaxBound
  , GBounded()

    -- ** 'Functor'
  , gfmap
  , gconstmap

    -- ** 'Foldable'
  , gfoldMap
  , gfoldr

    -- ** 'Traversable'
  , gtraverse
  , gsequenceA

    -- * Newtypes
  , Generically(..)
  , Generically1(..)

    -- * Accessing metadata
  , typeName
  ) where

import Generic.Data.Internal.Prelude
import Generic.Data.Internal.Enum
import Generic.Data.Internal.Meta
import Generic.Data.Internal.Show
import Generic.Data.Internal.Newtype
