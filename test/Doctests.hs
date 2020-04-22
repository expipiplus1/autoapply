module Main where

import Build_doctests (flags, pkgs)
import Test.DocTest

main :: IO ()
main = doctest $ flags <> pkgs <> ["-fno-print-bind-result", "test/Types.hs"]
