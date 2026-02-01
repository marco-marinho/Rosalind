module Perm (perm) where

import Data.List (permutations)

perm :: String -> String
perm input = ostr
  where
    endVal = read input :: Int
    perms = permutations [1 .. endVal]
    len = length perms
    olines = map (unwords . map show) perms
    ostr = unlines (show len : olines)