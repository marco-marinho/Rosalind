module Lcsm (lcsm) where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (find, inits, isInfixOf, sortBy, tails)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Util (parseInput, sequenceData)

allSubstrings :: String -> [String]
allSubstrings str = Set.toList $ Set.fromList [sub | t <- tails str, sub <- inits t, not (null sub)]

isCommonSubstring :: [String] -> String -> Bool
isCommonSubstring sequences sub = all (sub `isInfixOf`) sequences

lcsm :: String -> String
lcsm input = lcsmSub
  where
    fastas = map sequenceData $ parseInput input
    base = minimumBy (compare `on` length) fastas
    substrings = sortBy (flip compare `on` length) (allSubstrings base)
    lcsmSub = fromMaybe "" $ find (isCommonSubstring fastas) substrings
