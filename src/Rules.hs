module Rules ( hasRule, getRule ) where
import Data.Maybe (isNothing)

hasRule :: Int -> Bool
hasRule n | isNothing (getRule n) = False
          | otherwise = True

getRule :: Int -> Maybe ((Bool, Bool, Bool) -> Bool)
getRule 30  = Just rule30
getRule 90  = Just rule90
getRule 110 = Just rule110
getRule _   = Nothing

rule30 :: (Bool, Bool, Bool) -> Bool
rule30 (True, True, True) = False
rule30 (True, True, False) = False
rule30 (True, False, True) = False
rule30 (True, False, False) = True
rule30 (False, True, True) = True
rule30 (False, True, False) = True
rule30 (False, False, True) = True
rule30 (False, False, False) = False


rule90 :: (Bool, Bool, Bool) -> Bool
rule90 (True, True, True) = False
rule90 (True, True, False) = True
rule90 (True, False, True) = False
rule90 (True, False, False) = True
rule90 (False, True, True) = True
rule90 (False, True, False) = False
rule90 (False, False, True) = True
rule90 (False, False, False) = False


rule110 :: (Bool, Bool, Bool) -> Bool
rule110 (True, True, True) = False
rule110 (True, True, False) = True
rule110 (True, False, True) = True
rule110 (True, False, False) = True
rule110 (False, True, True) = True
rule110 (False, True, False) = True
rule110 (False, False, True) = False
rule110 (False, False, False) = False
