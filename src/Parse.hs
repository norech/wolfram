module Parse ( Args(..), parseArgs ) where

import Text.Read (readMaybe)
import Rules (hasRule)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)

data Args = Args {
    rule :: Int,
    start :: Int,
    width :: Int,
    height :: Int,
    offset :: Int
} deriving (Show)

readPositiveInt :: String -> Maybe Int
readPositiveInt s = readMaybe s >>= positive
    where positive a | a >= 0 = Just a
                     | otherwise = Nothing

parseNextArg :: Args -> [String] -> Maybe Args
parseNextArg args [] = Just args
parseNextArg args ("--rule":r:xs)
    = readMaybe r >>= \r -> if hasRule r
        then parseNextArg (args { rule = r }) xs else Nothing
parseNextArg args ("--start":s:xs)
    = readPositiveInt s >>= \s -> parseNextArg (args { start = s }) xs
parseNextArg args xs = parseNextArg' args xs

parseNextArg' :: Args -> [String] -> Maybe Args
parseNextArg' args ("--window":w:xs)
    = readPositiveInt w >>= \w -> parseNextArg (args { width = w }) xs
parseNextArg' args ("--lines":h:xs)
    = readPositiveInt h >>= \h -> parseNextArg (args { height = h }) xs
parseNextArg' args ("--move":m:xs)
    = readMaybe m >>= \m -> parseNextArg (args { offset = m }) xs
parseNextArg' _ _ = Nothing

exitWithHelp :: IO Args
exitWithHelp = hPutStrLn stderr ("Usage: ./wolfram [--rule r] [--start s]" ++
                " [--window w] [--lines h] [--move m]")
                >> exitWith (ExitFailure 84)

parseArgs :: [String] -> IO Args
parseArgs l = case o l of
    Nothing -> exitWithHelp
    Just a | rule a == -1 -> exitWithHelp
    Just a -> pure a
    where o [] = Nothing
          o l = parseNextArg (Args (-1) 0 80 (-1) 0) l
