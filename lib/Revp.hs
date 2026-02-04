module Revp (revp) where

import Data.Vector qualified as V
import Util (parseInput, sequenceData)

isComplement :: Char -> Char -> Bool
isComplement 'A' 'T' = True
isComplement 'T' 'A' = True
isComplement 'C' 'G' = True
isComplement 'G' 'C' = True
isComplement _ _ = False

getNested :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getNested (_, l) acc | l < 4 = acc
getNested (pos, len) acc = getNested (pos + 1, len - 2) ((pos, len) : acc)

palindLen :: V.Vector Char -> (Int, Int) -> Int -> Int
palindLen _ _ 12 = 12
palindLen gens (l, r) len
  | l < 0 || r >= V.length gens = len
  | not (isComplement (gens V.! l) (gens V.! r)) = len
  | otherwise = palindLen gens (l - 1, r + 1) (len + 2)

findPalindromes :: V.Vector Char -> Int -> [(Int, Int)] -> [(Int, Int)]
findPalindromes gens idx acc | idx >= V.length gens = acc
findPalindromes gens idx acc = findPalindromes gens (idx + 1) nacc
  where
    len = palindLen gens (idx, idx + 1) 0
    nacc = if len >= 4 then getNested (idx - (len `div` 2) + 2, len) acc else acc

revp :: String -> String
revp input = res
  where
    fastas = parseInput input
    fasta = case fastas of
      [f] -> f
      _ -> error "Expected exactly one FASTA sequence"
    seqData = V.fromList (sequenceData fasta)
    palindromes = findPalindromes seqData 0 []
    res = unlines $ map (\(pos, len) -> show pos ++ " " ++ show len) palindromes
