module Fibd (fibd) where

import Data.Maybe (fromMaybe)

lookupOrZero :: Int -> [(Int, Integer)] -> Integer
lookupOrZero _ [] = 0
lookupOrZero key ilst = Data.Maybe.fromMaybe 0 (lookup key ilst)

parse :: [Char] -> (Int, Int)
parse s = (n, m)
  where
    (n, m) = case words s of
      [nStr, mStr] -> (read nStr :: Int, read mStr :: Int)
      _ -> error "Invalid input format"

fib' :: Int -> Int -> Int -> [(Int, Integer)] -> Integer
fib' n tgt _ mem | n == tgt = lookupOrZero (n - 1) mem
fib' n tgt m mem = fib' (n + 1) tgt m newMem
  where
    prev1 = lookupOrZero (n - 1) mem
    prev2 = lookupOrZero (n - 2) mem
    dead = if n == m then 1 else lookupOrZero (n - (m + 1)) mem
    newVal = prev1 + prev2 - dead
    newMem = (n, newVal) : mem

fibd :: String -> String
fibd s = show $ fib' 2 n m [(1, 1), (0, 1)]
  where
    (n, m) = parse s
