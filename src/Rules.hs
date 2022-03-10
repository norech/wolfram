module Rules ( hasRule, getRule ) where
import Data.Maybe (isNothing)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary 255 = [1, 1, 1, 1, 1, 1, 1, 1]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

makeRule :: [Int] -> (Bool, Bool, Bool) -> Bool
makeRule tr (True, True, True) = toBool $ head tr
makeRule tr (True, True, False) = toBool $ tr !! 1
makeRule tr (True, False, True) = toBool $ tr !! 2
makeRule tr (True, False, False) = toBool $ tr !! 3
makeRule tr (False, True, True) = toBool $ tr !! 4
makeRule tr (False, True, False) = toBool $ tr !! 5
makeRule tr (False, False, True) = toBool $ tr !! 6
makeRule tr (False, False, False) = toBool $ tr !! 7

getRuleTransform :: Int -> [Int]
getRuleTransform n = replicate (8 - length b') 0 ++ b'
    where b' = toBinary n

hasRule :: Int -> Bool
hasRule n | isNothing (getRule n) = False
          | otherwise = True

getRule :: Int -> Maybe ((Bool, Bool, Bool) -> Bool)
getRule n | n < 0 || n > 255 = Nothing
          | otherwise = Just $ makeRule $ getRuleTransform n
