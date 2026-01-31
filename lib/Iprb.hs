module Iprb (mendelLaw) where

calculateProb :: Double -> Double -> Double -> Double
calculateProb k m n = 1 - probRecessive
  where
    total = k + m + n
    mWithm =(m * (m - 1)) * 0.25
    mWithn = (m * n + n * m) * 0.5
    nWithn = n * (n - 1)
    probRecessive = (mWithm + mWithn + nWithn) / (total * (total - 1))

mendelLaw :: String -> String
mendelLaw s = case words s of
  [kStr, mStr, nStr] -> show $ calculateProb k m n
    where
      k = read kStr :: Double
      m = read mStr :: Double
      n = read nStr :: Double
  _ -> error "Invalid input format"

