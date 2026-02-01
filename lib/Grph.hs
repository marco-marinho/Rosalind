module Grph (overlap) where

import Util (Fasta, header, parseInput, sequenceData)

buildGraph :: [(Fasta, Fasta)] -> [(String, String)] -> [(String, String)]
buildGraph [] acc = reverse acc
buildGraph ((f1, f2) : xs) acc = buildGraph xs newAcc
  where
    a = sequenceData f1
    b = sequenceData f2
    hasOverlap = take 3 a == drop (length b - 3) b
    newAcc = if hasOverlap then (header f1, header f2) : acc else acc

overlap :: String -> String
overlap input = olines
  where
    fastas = parseInput input
    pairs = [(x, y) | x <- fastas, y <- fastas, x /= y]
    graph = buildGraph pairs []
    olines = unlines [h2 ++ " " ++ h1 | (h1, h2) <- graph]