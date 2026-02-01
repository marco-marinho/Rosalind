module Lcsm (lcsm) where

import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.List (inits, isInfixOf, tails)
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
    substrings = allSubstrings base
    commonSubs = filter (isCommonSubstring fastas) substrings
    lcsmSub = maximumBy (compare `on` length) commonSubs
