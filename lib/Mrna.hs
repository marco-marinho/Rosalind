module Mrna (mrna) where

import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Util (Amino (Amino, Stop), codonsMap)

getAmino :: Amino -> Char
getAmino amino =
  case amino of
    Amino c -> c
    Stop -> ' '

mrna :: String -> String
mrna input = show ((res * 3) `mod` 1000000)
  where
    aminos = map (getAmino . snd) codonsMap
    aminosCounts = [(x, toInteger (length g)) | g@(x : _) <- group (sort aminos)]
    codonCounts = map (\x -> fromMaybe 0 (lookup x aminosCounts)) input
    res = product codonCounts