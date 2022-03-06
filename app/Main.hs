module Main where

import System.Environment ( getArgs )

import Lib ( generateGrid )
import Parse ( parseArgs )

main :: IO ()
main = getArgs >>= parseArgs >>= generateGrid
