module Prot (translate) where

import Util (Amino (Amino, Stop), codonsMap)

translate' :: [Char] -> [Char] -> [Char]
translate' rna acc
  | null rna = reverse acc
  | otherwise =
      let (codon, rest) = splitAt 3 rna
          aminoAcid = lookup codon codonsMap
       in case aminoAcid of
            Just Stop -> reverse acc
            Just (Amino a) -> translate' rest (a : acc)
            Nothing -> error "Invalid codon"

translate :: [Char] -> [Char]
translate rna = translate' rna []
