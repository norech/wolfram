module Lib ( generateGrid ) where

import Rules (getRule)
import Parse (Args(..))
import Data.Maybe (fromJust, isJust)
import Control.Monad (replicateM_)

getIndexOrFalse :: Int -> [Bool] -> Bool
getIndexOrFalse i xs | (i >= 0) && (i < length xs) = xs !! i
getIndexOrFalse i xs = False

getAboveCells :: Int -> [Bool] -> (Bool, Bool, Bool)
getAboveCells i xs = (
    getIndexOrFalse (i - 2) xs,
    getIndexOrFalse (i - 1) xs,
    getIndexOrFalse i xs)

getValue :: Args -> Int -> [Bool] -> Bool
getValue a i prev | isJust curRule = fromJust curRule (getAboveCells i prev)
    where curRule = getRule (aRule a)
getValue _ _ _ = False

getLineSize :: Args -> Int -> Int
getLineSize a numLine = 2 * aWindow a + 2 * numLine + 2

generateLine :: Args -> Int -> [Bool] -> IO (Maybe [Bool])
generateLine a i _ | aLines a /= -1 && i > aLines a + aStart a = pure Nothing
generateLine a i prevLine = printLine a i line >> pure (Just line)
    where line = [getValue a x prevLine | x <- [0..(getLineSize a i)]]

printBool :: Bool -> IO ()
printBool True  = putChar '*'
printBool False = putChar ' '

truncateAtMiddle :: Int -> Int -> [Bool] -> [Bool]
truncateAtMiddle n o xs = take n $ drop (maxwidth - o) xs
    where maxwidth = length xs `div` 2 - n `div` 2

printLine :: Args -> Int -> [Bool] -> IO ()
printLine a i _ | i <= aStart a = pure ()
printLine a i xs
    = mapM_ printBool (truncateAtMiddle (aWindow a) (aOffset a) xs)
    >> putStrLn ""

generateNextLines :: Args -> Int -> Maybe [Bool] -> IO [Bool]
generateNextLines a i (Just xs) = generateLines a (i + 1) xs
generateNextLines _ _ _ = pure []

generateLines :: Args -> Int -> [Bool] -> IO [Bool]
generateLines a i xs = generateLine a i xs >>= generateNextLines a i

generateFirstLineCell :: Args -> Int -> Bool
generateFirstLineCell a i | i == getLineSize a 1 `div` 2 = True
                          | otherwise = False

generateGrid :: Args -> IO ()
generateGrid a = printLine a 1 prev >> generateLines a 2 prev >> pure ()
    where prev = [generateFirstLineCell a x | x <- [0..(getLineSize a 1)]]
