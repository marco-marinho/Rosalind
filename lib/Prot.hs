module Prot (translate) where

data Amino = Stop | Amino Char deriving (Show)

codons :: [(String, Amino)]
codons = 
    [ ("UUU", Amino 'F'), ("CUU", Amino 'L'), ("AUU", Amino 'I'), ("GUU", Amino 'V')
    , ("UUC", Amino 'F'), ("CUC", Amino 'L'), ("AUC", Amino 'I'), ("GUC", Amino 'V')
    , ("UUA", Amino 'L'), ("CUA", Amino 'L'), ("AUA", Amino 'I'), ("GUA", Amino 'V')
    , ("UUG", Amino 'L'), ("CUG", Amino 'L'), ("AUG", Amino 'M'), ("GUG", Amino 'V')
    , ("UCU", Amino 'S'), ("CCU", Amino 'P'), ("ACU", Amino 'T'), ("GCU", Amino 'A')
    , ("UCC", Amino 'S'), ("CCC", Amino 'P'), ("ACC", Amino 'T'), ("GCC", Amino 'A')
    , ("UCA", Amino 'S'), ("CCA", Amino 'P'), ("ACA", Amino 'T'), ("GCA", Amino 'A')
    , ("UCG", Amino 'S'), ("CCG", Amino 'P'), ("ACG", Amino 'T'), ("GCG", Amino 'A')
    , ("UAU", Amino 'Y'), ("CAU", Amino 'H'), ("AAU", Amino 'N'), ("GAU", Amino 'D')
    , ("UAC", Amino 'Y'), ("CAC", Amino 'H'), ("AAC", Amino 'N'), ("GAC", Amino 'D')
    , ("UAA", Stop),      ("CAA", Amino 'Q'), ("AAA", Amino 'K'), ("GAA", Amino 'E')
    , ("UAG", Stop),      ("CAG", Amino 'Q'), ("AAG", Amino 'K'), ("GAG", Amino 'E')
    , ("UGU", Amino 'C'), ("CGU", Amino 'R'), ("AGU", Amino 'S'), ("GGU", Amino 'G')
    , ("UGC", Amino 'C'), ("CGC", Amino 'R'), ("AGC", Amino 'S'), ("GGC", Amino 'G')
    , ("UGA", Stop),      ("CGA", Amino 'R'), ("AGA", Amino 'R'), ("GGA", Amino 'G')
    , ("UGG", Amino 'W'), ("CGG", Amino 'R'), ("AGG", Amino 'R'), ("GGG", Amino 'G')]

translate' :: [Char] -> [Char] -> [Char]
translate' rna acc 
    | null rna = reverse acc
    | otherwise =
        let (codon, rest) = splitAt 3 rna
            aminoAcid = lookup codon codons
        in case aminoAcid of
            Just Stop -> reverse acc
            Just (Amino a) -> translate' rest (a : acc)
            Nothing -> error "Invalid codon"

translate :: [Char] -> [Char]
translate rna = translate' rna []
