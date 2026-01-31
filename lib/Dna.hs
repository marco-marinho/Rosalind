module Dna (countNucleotides) where

countNucleotides' :: [Char] -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
countNucleotides' [] acc = acc
countNucleotides' (x:xs) (a, c, g, t) = case x of
    'A' -> countNucleotides' xs (a + 1, c, g, t)
    'C' -> countNucleotides' xs (a, c + 1, g, t)
    'G' -> countNucleotides' xs (a, c, g + 1, t)
    'T' -> countNucleotides' xs (a, c, g, t + 1)
    _   -> countNucleotides' xs (a, c, g, t)

countNucleotides :: [Char] -> String
countNucleotides dna = show $ countNucleotides' dna (0, 0, 0, 0)

