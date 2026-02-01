module Gc (solveGc) where

import Data.Foldable (maximumBy)
import Util (Fasta, header, parseInput, sequenceData)

gcCount :: String -> Int -> Int
gcCount [] gcs = gcs
gcCount (x : xs) gcs = case x of
  'G' -> gcCount xs (gcs + 1)
  'C' -> gcCount xs (gcs + 1)
  _ -> gcCount xs gcs

gcContent :: Fasta -> Double
gcContent fasta = gcRatio
  where
    seqData = sequenceData fasta
    gcNum = gcCount seqData 0
    totalNum = length seqData
    gcRatio = (fromIntegral gcNum / fromIntegral totalNum) * 100.0

highestGcContent :: [Fasta] -> (String, Double)
highestGcContent fastas = bestFasta
  where
    gcContents = [(header f, gcContent f) | f <- fastas]
    bestFasta = maximumBy (\(_, g1) (_, g2) -> compare g1 g2) gcContents

solveGc :: String -> String
solveGc input = show $ highestGcContent (parseInput input)