module Lia (lia) where

binomialCoefficient :: Integer -> Integer -> Integer
binomialCoefficient n k = factorial n `div` (factorial k * factorial (n - k))
  where
    factorial 0 = 1
    factorial m = m * factorial (m - 1)

binomialProbability :: Integer -> Integer -> Double -> Double
binomialProbability n k p = fromIntegral (binomialCoefficient n k) * (p ** fromIntegral k) * ((1 - p) ** fromIntegral (n - k))

binomialCDFAtLeast :: Integer -> Integer -> Double
binomialCDFAtLeast n k = sum [binomialProbability n i 0.25 | i <- [k .. n]]

lia :: String -> String
lia input = case words input of
  [aStr, bStr] ->
    let generation = read aStr :: Integer
        nOrganisms = read bStr :: Integer
        result = binomialCDFAtLeast (2 ^ generation) nOrganisms
     in show result
  _ -> error "Invalid input format. Expected two integers."