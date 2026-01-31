module Revc (reverseComplement) where

complement :: [Char] -> [Char] -> [Char]
complement [] acc = reverse acc
complement (x:xs) acc = case x of
    'A' -> complement xs ('T' : acc)
    'T' -> complement xs ('A' : acc)
    'C' -> complement xs ('G' : acc)
    'G' -> complement xs ('C' : acc)
    _   -> complement xs acc

reverseComplement :: [Char] -> [Char]
reverseComplement dna = complement (reverse dna) []