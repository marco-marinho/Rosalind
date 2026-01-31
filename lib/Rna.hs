module Rna (transcribe) where

transcribe' :: [Char] -> [Char] -> [Char]
transcribe' [] acc = reverse acc
transcribe' (x:xs) acc = case x of
    'T' -> transcribe' xs ('U' : acc)
    _   -> transcribe' xs (x : acc)

transcribe :: [Char] -> [Char]
transcribe dna = transcribe' dna []