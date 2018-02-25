-- | Generic deriving for standard classes in base

{-# LANGUAGE FlexibleContexts #-}

module Generic.Data.Prelude where

import Control.Applicative (liftA2, Alternative(..))
import Data.Function (on)
import Data.Semigroup
import GHC.Generics

-- * 'Eq'

-- | Generic @('==')@.
--
-- @
-- instance 'Eq' MyType where
--   ('==') = 'geq'
-- @
geq :: (Generic a, Eq (Rep a ())) => a -> a -> Bool
geq = (==) `on` from'

-- * 'Ord'

-- | Generic 'compare'.
--
-- @
-- instance 'Ord' MyType where
--   'compare' = 'gcompare'
-- @
gcompare :: (Generic a, Ord (Rep a ())) => a -> a -> Ordering
gcompare = compare `on` from'

-- * 'Semigroup'

-- | Generic @('<>')@ (or 'mappend').
--
-- @
-- instance 'Semigroup' MyType where
--   ('<>') = 'gmappend'
-- @
--
-- See also 'gmempty'.
gmappend :: (Generic a, Semigroup (Rep a ())) => a -> a -> a
gmappend a b = to (from' a <> from' b)

-- * 'Monoid'

-- | Generic 'mempty'.
--
-- @
-- instance 'Monoid' MyType where
--   'mempty' = 'gmempty'
-- @
gmempty :: (Generic a, Monoid (Rep a ())) => a
gmempty = to' mempty

-- * 'Functor'

-- | Generic 'fmap'.
--
-- @
-- instance 'Functor' MyTypeF where
--   'fmap' = 'gfmap'
-- @
gfmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
gfmap f = to1 . fmap f . from1

-- | Generic @('<$')@.
--
-- See also 'gfmap'.
gconstmap :: (Generic1 f, Functor (Rep1 f)) => a -> f b -> f a
gconstmap a = to1 . (a <$) . from1

-- * 'Applicative'

-- | Generic 'pure'.
--
-- @
-- instance 'Applicative' MyTypeF where
--   'pure' = 'gpure'
--   ('<*>') = 'gap'
-- @
gpure :: (Generic1 f, Applicative (Rep1 f)) => a -> f a
gpure = to1 . pure

-- | Generic @('<*>')@ (or 'Control.Monad.ap').
--
-- See also 'gpure'.
gap :: (Generic1 f, Applicative (Rep1 f)) => f (a -> b) -> f a -> f b
gap = liftG2 (<*>)

-- | Generic 'liftA2'.
--
-- See also 'gpure'.
gliftA2 :: (Generic1 f, Applicative (Rep1 f)) => (a -> b -> c) -> f a -> f b -> f c
gliftA2 = liftG2 . liftA2

-- * 'Alternative'

-- | Generic 'empty'.
--
-- @
-- instance 'Alternative' MyTypeF where
--   'empty' = 'gempty'
--   ('<|>') = 'galt'
-- @
gempty :: (Generic1 f, Alternative (Rep1 f)) => f a
gempty = to1 empty

-- | Generic ('<|>').
--
-- See also 'gempty'.
galt :: (Generic1 f, Alternative (Rep1 f)) => f a -> f a -> f a
galt = liftG2 (<|>)

-- * 'Foldable'

-- | Generic 'foldMap'.
--
-- @
-- instance 'Foldable' MyTypeF where
--   'foldMap' = 'gfoldMap'
-- @
gfoldMap :: (Generic1 f, Foldable (Rep1 f), Monoid m) => (a -> m) -> f a -> m
gfoldMap f = foldMap f . from1

-- | Generic 'foldr'.
--
-- @
-- instance 'Foldable' MyTypeF where
--   'foldr' = 'gfoldr'
-- @
--
-- See also 'gfoldMap'.
gfoldr :: (Generic1 f, Foldable (Rep1 f)) => (a -> b -> b) -> b -> f a -> b
gfoldr f b = foldr f b . from1

-- * 'Traversable'

-- | Generic 'traverse'.
--
-- @
-- instance 'Traversable' MyTypeF where
--   'traverse' = 'gtraverse'
-- @
gtraverse
  :: (Generic1 f, Traversable (Rep1 f), Applicative m)
  => (a -> m b) -> f a -> m (f b)
gtraverse f = fmap to1 . traverse f . from1

-- | Generic 'sequenceA'.
--
-- @
-- instance 'Traversable' MyTypeF where
--   'sequenceA' = 'gsequenceA'
-- @
--
-- See also 'gtraverse'.
gsequenceA
  :: (Generic1 f, Traversable (Rep1 f), Applicative m)
  => f (m a) -> m (f a)
gsequenceA = fmap to1 . sequenceA . from1

-- * Utils

-- | A helper for better type inference.
from' :: Generic a => a -> Rep a ()
from' = from

-- | A helper for better type inference.
to' :: Generic a => Rep a () -> a
to' = to

-- | Lift binary combinators generically.
liftG2 :: Generic1 f => (Rep1 f a -> Rep1 f b -> Rep1 f c) -> f a -> f b -> f c
liftG2 (<?>) a b = to1 (from1 a <?> from1 b)
