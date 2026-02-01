module Subs (motif) where

startsWith :: String -> String -> Bool
startsWith [] [] = True
startsWith _ [] = True
startsWith [] _ = False
startsWith (x : xs) (y : ys) = (x == y) && startsWith xs ys

motif' :: String -> String -> Int -> [Int] -> [Int]
motif' [] _ _ acc = reverse acc
motif' str prefix j acc
  | startsWith str prefix = motif' (drop 1 str) prefix (j + 1) (j : acc)
  | otherwise = motif' (drop 1 str) prefix (j + 1) acc

motif :: String -> String
motif input = case lines input of
  [s, t] -> unwords $ map show $ motif' s t 1 []
  _ -> error "Invalid input format"