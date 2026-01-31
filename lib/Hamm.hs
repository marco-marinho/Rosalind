module Hamm (mutations) where

mutations' :: [Char] -> [Char] -> Int -> Int
mutations' [] _ acc = acc
mutations' _ [] acc = acc
mutations' (x : xs) (y : ys) acc = case x == y of
  True -> mutations' xs ys acc
  False -> mutations' xs ys (acc + 1)

mutations :: String -> String
mutations s = show $ mutations' s1 s2 0
  where
    (s1, s2) = case lines s of
      (a : b : _) -> (a, b)
      _ -> error "Invalid input format"
