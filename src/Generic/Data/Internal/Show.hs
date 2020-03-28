{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}

-- | Generic implementation of Show
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
--
-- If something here seems useful, please report it or create a pull request to
-- export it from an external module.

module Generic.Data.Internal.Show where

import Data.Foldable (foldl')
import Data.Functor.Classes (Show1(..))
import Data.Functor.Identity
import Data.Proxy
import Generic.Data.Internal.Utils (isSymDataCon, isSymVar)
import GHC.Generics
import Text.Show.Combinators

-- | Generic 'showsPrec'.
--
-- @
-- instance 'Show' MyType where
--   'showsPrec' = 'gshowsPrec'
-- @
gshowsPrec :: (Generic a, GShow0 (Rep a)) => Int -> a -> ShowS
gshowsPrec = flip gprecShows

gprecShows :: (Generic a, GShow0 (Rep a)) => a -> PrecShowS
gprecShows = gPrecShows Proxy . from

-- | Generic representation of 'Show' types.
type GShow0 = GShow Proxy

-- | Generic 'liftShowsPrec'.
gliftShowsPrec
  :: (Generic1 f, GShow1 (Rep1 f))
  => (Int -> a -> ShowS) -> ([a] -> ShowS)
  -> Int -> f a -> ShowS
gliftShowsPrec showsPrec' showList' =
  flip (gLiftPrecShows showsPrec' showList' . from1)

gLiftPrecShows
  :: GShow1 f
  => (Int -> a -> ShowS) -> ([a] -> ShowS)
  -> f a -> PrecShowS
gLiftPrecShows = curry (gPrecShows . Identity)

type ShowsPrec a = (Int -> a -> ShowS, [a] -> ShowS)

-- | Generic representation of 'Data.Functor.Classes.Show1' types.
type GShow1 = GShow Identity

class GShow p f where
  gPrecShows :: p (ShowsPrec a) -> f a -> PrecShowS

instance GShow p f => GShow p (M1 D d f) where
  gPrecShows p (M1 x) = gPrecShows p x

instance (GShow p f, GShow p g) => GShow p (f :+: g) where
  gPrecShows p (L1 x) = gPrecShows p x
  gPrecShows p (R1 y) = gPrecShows p y

instance (Constructor c, GShowC p c f) => GShow p (M1 C c f) where
  gPrecShows p x = gPrecShowsC p (conName x) (conFixity x) x

instance GShow p V1 where
  gPrecShows _ v = case v of {}

class GShowC p c f where
  gPrecShowsC :: p (ShowsPrec a) -> String -> Fixity -> M1 C c f a -> PrecShowS

instance GShowFields p f => GShowC p ('MetaCons s y 'False) f where
  gPrecShowsC p name fixity (M1 x)
    | Infix _ fy <- fixity, k1 : k2 : ks <- fields =
      foldl' showApp (showInfix cname fy k1 k2) ks
    | otherwise = foldl' showApp (showCon cname) fields
    where
      cname = surroundConName fixity name
      fields = gPrecShowsFields p x

instance GShowNamed p f => GShowC p ('MetaCons s y 'True) f where
  gPrecShowsC p name fixity (M1 x) = showRecord cname fields
    where
      cname = surroundConName fixity name
      fields = gPrecShowsNamed p x

class GShowFields p f where
  gPrecShowsFields :: p (ShowsPrec a) -> f a -> [PrecShowS]

instance (GShowFields p f, GShowFields p g) => GShowFields p (f :*: g) where
  gPrecShowsFields p (x :*: y) = gPrecShowsFields p x ++ gPrecShowsFields p y

instance GShowSingle p f => GShowFields p (M1 S c f) where
  gPrecShowsFields p (M1 x) = [gPrecShowsSingle p x]

instance GShowFields p U1 where
  gPrecShowsFields _ U1 = []

class GShowNamed p f where
  gPrecShowsNamed :: p (ShowsPrec a) -> f a -> ShowFields

instance (GShowNamed p f, GShowNamed p g) => GShowNamed p (f :*: g) where
  gPrecShowsNamed p (x :*: y) = gPrecShowsNamed p x &| gPrecShowsNamed p y

instance (Selector c, GShowSingle p f) => GShowNamed p (M1 S c f) where
  gPrecShowsNamed p x'@(M1 x) = snameParen `showField` gPrecShowsSingle p x
    where
      sname = selName x'
      snameParen | isSymVar sname = "(" ++ sname ++ ")"
                 | otherwise      = sname

instance GShowNamed p U1 where
  gPrecShowsNamed _ U1 = noFields

class GShowSingle p f where
  gPrecShowsSingle :: p (ShowsPrec a) -> f a -> PrecShowS

instance Show a => GShowSingle p (K1 i a) where
  gPrecShowsSingle _ (K1 x) = flip showsPrec x

instance Show1 f => GShowSingle Identity (Rec1 f) where
  gPrecShowsSingle (Identity sp) (Rec1 r) =
    flip (uncurry liftShowsPrec sp) r

instance GShowSingle Identity Par1 where
  gPrecShowsSingle (Identity (showsPrec', _)) (Par1 a) = flip showsPrec' a

instance (Show1 f, GShowSingle p g)
  => GShowSingle p (f :.: g) where
  gPrecShowsSingle p (Comp1 c) =
      flip (liftShowsPrec showsPrec_ showList_) c
    where
      showsPrec_ = flip (gPrecShowsSingle p)
      showList_ = showListWith (showsPrec_ 0)

-- Helpers

surroundConName :: Fixity -> String -> String
surroundConName fixity name =
  case fixity of
    Prefix
      | isSymName -> "(" ++ name ++ ")"
      | otherwise -> name
    Infix _ _
      | isSymName -> name
      | otherwise -> "`" ++ name ++ "`"
  where
    isSymName = isSymDataCon name
