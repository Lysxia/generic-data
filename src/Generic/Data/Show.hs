{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}

module Generic.Data.Show (
    gshowsPrec
  , gprecShows
  , GShow(..)
  ) where

import Data.Foldable (foldl')
import GHC.Generics
import Text.Show.Combinators

-- | Default definition
gshowsPrec :: (Generic a, GShow (Rep a)) => Int -> a -> ShowS
gshowsPrec = flip gprecShows

gprecShows :: (Generic a, GShow (Rep a)) => a -> PrecShowS
gprecShows = gPrecShows . from

-- | Internal 'Show' class
class GShow f where
  gPrecShows :: f a -> PrecShowS

instance GShow f => GShow (M1 D d f) where
  gPrecShows (M1 x) = gPrecShows x

instance (GShow f, GShow g) => GShow (f :+: g) where
  gPrecShows (L1 x) = gPrecShows x
  gPrecShows (R1 y) = gPrecShows y

instance (Constructor c, GShowC c f) => GShow (M1 C c f) where
  gPrecShows x = gPrecShowsC (conName x) (conFixity x) x

instance GShow V1 where
  gPrecShows v = case v of {}

class GShowC c f where
  gPrecShowsC :: String -> Fixity -> M1 C c f p -> PrecShowS

instance GShowFields f => GShowC ('MetaCons s y 'False) f where
  gPrecShowsC name fixity (M1 x)
    | Infix _ fy <- fixity, k1 : k2 : ks <- fields =
      foldl' showApp (showInfix name fy k1 k2) ks
    | otherwise = foldl' showApp (showCon cname) fields
    where
      cname = case fixity of
        Prefix -> name
        Infix _ _ -> "(" ++ name ++ ")"
      fields = gPrecShowsFields x

instance GShowNamed f => GShowC ('MetaCons s y 'True) f where
  gPrecShowsC name fixity (M1 x) = showRecord cname fields
    where
      cname = case fixity of
        Prefix -> name
        Infix _ _ -> "(" ++ name ++ ")"
      fields = gPrecShowsNamed x

class GShowFields f where
  gPrecShowsFields :: f p -> [PrecShowS]

instance (GShowFields f, GShowFields g) => GShowFields (f :*: g) where
  gPrecShowsFields (x :*: y) = gPrecShowsFields x ++ gPrecShowsFields y

instance GShowSingle f => GShowFields (M1 S c f) where
  gPrecShowsFields (M1 x) = [gPrecShowsSingle x]

instance GShowFields U1 where
  gPrecShowsFields U1 = []

class GShowNamed f where
  gPrecShowsNamed :: f p -> ShowFields

instance (GShowNamed f, GShowNamed g) => GShowNamed (f :*: g) where
  gPrecShowsNamed (x :*: y) = gPrecShowsNamed x &| gPrecShowsNamed y

instance (Selector c, GShowSingle f) => GShowNamed (M1 S c f) where
  gPrecShowsNamed x'@(M1 x) = selName x' `showField` gPrecShowsSingle x

instance GShowNamed U1 where
  gPrecShowsNamed U1 = noFields

class GShowSingle f where
  gPrecShowsSingle :: f p -> PrecShowS

instance Show a => GShowSingle (K1 i a) where
  gPrecShowsSingle (K1 x) = flip showsPrec x
