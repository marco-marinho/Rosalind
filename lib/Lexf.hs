module Lexf (lexf) where

import Control.Monad (replicateM)

lexf :: String -> String
lexf input = res
  where
    (symbols, n) = case lines input of
      [s, num] -> (filter (/= ' ') s, read num :: Int)
      _ -> error "Invalid input"
    res = unlines $ replicateM n symbols