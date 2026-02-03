module Mprt (mprt) where

findOcurrences :: String -> Int -> [Int] -> [Int]
findOcurrences str _ acc | length str < 4 = reverse acc
findOcurrences str idx acc = case str of
  (a : b : c : d : _) | a == 'N' && b /= 'P' && (c == 'S' || c == 'T') && d /= 'P' -> findOcurrences (drop 1 str) (idx + 1) (idx : acc)
  _ -> findOcurrences (drop 1 str) (idx + 1) acc

mprt :: [(String, String)] -> String
mprt input = ostring
  where
    res = filter (not . null . snd) $ map (\(protId, prot) -> (protId, unwords $ map show (findOcurrences prot 1 []))) input
    olines = map (\(protId, occs) -> protId ++ "\n" ++ occs) res
    ostring = unlines olines
