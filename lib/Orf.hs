module Orf (orf) where

import Data.List (nub)
import Data.Text qualified as T
import Revc (reverseComplement)
import Util (parseInput, sequenceData, translateTillStop)

orf :: String -> String
orf input = unlines res
  where
    fastas = parseInput input
    sqc = case fastas of
      x : _ -> T.replace (T.pack "T") (T.pack "U") (T.pack (sequenceData x))
      [] -> error "No FASTA sequences found."
    revSqc = case fastas of
      x : _ -> T.replace (T.pack "T") (T.pack "U") (T.pack (reverseComplement (sequenceData x)))
      [] -> error "No FASTA sequences found."
    toDecode = [T.unpack pos | (_, pos) <- T.breakOnAll (T.pack "AUG") sqc]
    decoded = map translateTillStop toDecode
    toDecodeRev = [T.unpack pos | (_, pos) <- T.breakOnAll (T.pack "AUG") revSqc]
    decodedRev = map translateTillStop toDecodeRev
    res = filter (not . null) $ nub (decoded ++ decodedRev)