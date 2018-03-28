-- Deriving instances for a "functor-functor"-style record.
-- (https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative (Alternative)
import Data.Coerce
import Data.Functor.Classes
import Data.Semigroup
import Data.Monoid (Alt(..))
import GHC.Generics (Generic)

import Generic.Data
import Generic.Data.Orphans ()

data MyRecord f = MyRecord
  { _field1 :: f Int
  , _field2 :: f Bool
  } deriving Generic

instance Show1 f => Show (MyRecord f) where
  showsPrec = coerce (gshowsPrec @(MyRecord (Id1 f)))

instance Eq1 f => Eq (MyRecord f) where
  (==) = coerce (geq @(MyRecord (Id1 f)))

instance Ord1 f => Ord (MyRecord f) where
  compare = coerce (gcompare @(MyRecord (Id1 f)))

instance Alternative f => Semigroup (MyRecord f) where
  (<>) = coerce (gmappend @(MyRecord (Alt f)))

instance Alternative f => Monoid (MyRecord f) where
  mempty = coerce (gmempty @(MyRecord (Alt f)))
  mappend = (<>)

main :: IO ()
main = return () -- Just make this compile
