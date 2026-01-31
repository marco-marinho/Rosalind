module Fib (fib) where

parse :: [Char] -> (Int, Int)
parse s = (n, k)
  where
    (n, k) = case words s of
        [nStr, kStr] -> (read nStr :: Int, read kStr :: Int)
        _ -> error "Invalid input format"

fib' ::  Int -> Int -> Int -> Int -> Int
fib' 0 _ _ b  = b
fib' n k a b = fib' (n - 1) k b (k * a + b)

fib :: String -> String
fib s = show $ fib' (n - 2) k 1 1
    where
        (n, k) = parse s