module Main where

import System.Exit ( ExitCode(ExitFailure), exitWith )

import Lib

main :: IO ()
main = exitWith $ ExitFailure 84
