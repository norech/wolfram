module Main where

import System.Environment ( getArgs )

import Lib ( generateGrid )
import Parse ( parseArgsOrExit )

main :: IO ()
main = getArgs >>= parseArgsOrExit >>= generateGrid
