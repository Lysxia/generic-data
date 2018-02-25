-- | Generic combinators to derive type class instances.

module Generic.Data
  ( -- * Deriving instances

    -- | Classes that GHC can not derive (excluding @GeneralizedNewtypeDeriving@):
    -- 'Data.Semigroup.Semigroup', 'Monoid', 'Applicative',
    -- 'Control.Applicative.Alternative', 'Data.Functor.Classes.Eq1',
    -- 'Data.Functor.Classes.Ord1'.
    --
    -- On base < 4.11 (i.e., GHC < 8.6), you must import "Generic.Data.Orphans"
    -- for generic deriving of 'Data.Semigroup.Semigroup' and 'Monoid'.

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

    -- * Other classes

    -- | GHC can already derive these.

    -- ** 'Eq'
  , geq

    -- ** 'Ord'
  , gcompare

    -- ** 'Show'
  , gshowsPrec
  , GShow()

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
