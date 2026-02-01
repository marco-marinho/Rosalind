module Iev (iev) where

calc :: Double -> Double -> Double -> Double -> Double -> Double -> Double
calc a b c d e f = expectedOffspring
  where
    total = a + b + c + d + e + f
    recessiveChance = (f + e * 0.5 + d * 0.25) / total
    dominantChance = 1 - recessiveChance
    expectedOffspring = 2 * total * dominantChance

iev :: String -> String
iev input = show res
  where
    vals = map (\s -> read s :: Double) (words input)
    res = case vals of
      [a, b, c, d, e, f] -> calc a b c d e f
      _ -> error "Invalid input format"