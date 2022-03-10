module Lib ( generateGrid ) where

import Rules (getRule)
import Parse (Args(..))
import Data.Maybe (fromJust, isJust)
import Control.Monad (replicateM_)

getIndexOrFalse :: Int -> [Bool] -> Bool
getIndexOrFalse n xs | (n >= 0) && (n < length xs) = xs !! n
getIndexOrFalse n xs = False

getAboveCells :: Int -> [Bool] -> (Bool, Bool, Bool)
getAboveCells n xs = (
    getIndexOrFalse (n - 2) xs,
    getIndexOrFalse (n - 1) xs,
    getIndexOrFalse n xs)

getValue :: Args -> Int -> [Bool] -> Bool
getValue a i prev | isJust curRule = fromJust curRule (getAboveCells i prev)
    where curRule = getRule (rule a)
getValue _ _ _ = False

getLineSize :: Args -> Int -> Int
getLineSize a n = 2 * width a + 2 * n + 2

generateLine :: Args -> Int -> [Bool] -> IO (Maybe [Bool])
generateLine a i _ | height a /= -1 && i > height a + start a = pure Nothing
generateLine a i prev = printLine a i l >> pure (Just l)
    where l = [getValue a x prev | x <- [0..(getLineSize a i)]]

printBool :: Bool -> IO ()
printBool True  = putChar '*'
printBool False = putChar ' '

truncateAtMiddle :: Int -> Int -> [Bool] -> [Bool]
truncateAtMiddle n o xs = take n $ drop ((length xs `div` 2 - n `div` 2) - o) xs

printLine :: Args -> Int -> [Bool] -> IO ()
printLine a i _ | i <= start a = pure ()
printLine a i xs
    = mapM_ printBool (truncateAtMiddle (width a) (offset a) xs)
    >> putStrLn ""

generateNextLines :: Args -> Int -> Maybe [Bool] -> IO [Bool]
generateNextLines a i (Just xs) = generateLines a (i + 1) xs
generateNextLines _ _ _ = pure []

generateLines :: Args -> Int -> [Bool] -> IO [Bool]
generateLines a i xs = generateLine a i xs >>= generateNextLines a i

generateFirstLineCell :: Args -> Int -> Bool
generateFirstLineCell a x | x == getLineSize a 1 `div` 2 = True
                          | otherwise = False

generateGrid :: Args -> IO ()
generateGrid a = printLine a 1 prev >> generateLines a 2 prev >> pure ()
    where prev = [generateFirstLineCell a x | x <- [0..(getLineSize a 1)]]
