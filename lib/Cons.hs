module Cons (consensus) where

import Util (parseInput, sequenceData)

countNucleotides :: [String] -> [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
countNucleotides [] counts = reverse $ drop 1 counts
countNucleotides dnas acc = countNucleotides next (counts : acc)
  where
    iacc = ((0, 0, 0, 0), [])
    (counts, next) =
      foldl
        ( \facc dna ->
            let ((a, c, g, t), lst) = facc
             in case take 1 dna of
                  "A" -> ((a + 1, c, g, t), drop 1 dna : lst)
                  "C" -> ((a, c + 1, g, t), drop 1 dna : lst)
                  "G" -> ((a, c, g + 1, t), drop 1 dna : lst)
                  "T" -> ((a, c, g, t + 1), drop 1 dna : lst)
                  _ -> facc
        )
        iacc
        dnas

bestNucleotide :: (Int, Int, Int, Int) -> String
bestNucleotide (a, c, g, t)
  | a >= c && a >= g && a >= t = "A"
  | c >= a && c >= g && c >= t = "C"
  | g >= a && g >= c && g >= t = "G"
  | otherwise = "T"

consensus :: String -> String
consensus input = profileStr
  where
    fastas = parseInput input
    dnaStrs = map sequenceData fastas
    counts = countNucleotides dnaStrs []
    consensusStr = concatMap bestNucleotide counts
    aCounts = unwords $ map (\(a, _, _, _) -> show a) counts
    cCounts = unwords $ map (\(_, c, _, _) -> show c) counts
    gCounts = unwords $ map (\(_, _, g, _) -> show g) counts
    tCounts = unwords $ map (\(_, _, _, t) -> show t) counts
    profileStr =
      unlines
        [ consensusStr,
          "A: " ++ aCounts,
          "C: " ++ cCounts,
          "G: " ++ gCounts,
          "T: " ++ tCounts
        ]