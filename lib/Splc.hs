module Splc (splc) where

import Data.String qualified as T
import Data.Text qualified as T
import Prot (translate)
import Util (parseInput, sequenceData)

splc :: String -> String
splc input = translate (T.unpack splicedRna)
  where
    fastas = parseInput input
    sequences = map (T.pack . sequenceData) fastas
    (dna, introns) = case sequences of
      (d : xs) -> (d, xs)
      [] -> error "No sequences found."
    splicedDna = foldl (\acc intron -> T.replace intron T.empty acc) dna introns
    splicedRna = T.replace (T.fromString "T") (T.fromString "U") splicedDna