module Main (main) where

import Test.DocTest (doctest)
import Build_doctests (flags, pkgs, module_sources)

main :: IO ()
main = doctest (flags ++ pkgs ++ module_sources)
