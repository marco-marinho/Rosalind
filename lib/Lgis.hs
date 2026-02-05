module Lgis (lgis) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Vector.Unboxed qualified as V

binarySearch :: STUArray s Int Int -> V.Vector Int -> Int -> Int -> Int -> ST s Int
binarySearch tails vec val low high
  | low > high = return low
  | otherwise = do
      let mid = (low + high) `div` 2
      idx <- readArray tails mid
      let midVal = vec V.! idx
      if midVal >= val
        then binarySearch tails vec val low (mid - 1)
        else binarySearch tails vec val (mid + 1) high

solve :: [Int] -> [Int]
solve [] = []
solve list = runST $ do
  let vec = V.fromList list
  let n = V.length vec

  tails <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
  parents <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
  lenRef <- newArray (0, 0) 0 :: ST s (STUArray s Int Int)

  writeArray tails 0 0
  writeArray lenRef 0 1

  forM_ [1 .. n - 1] $ \i -> do
    let x = vec V.! i
    currLen <- readArray lenRef 0
    lastTailIdx <- readArray tails (currLen - 1)
    let lastTailVal = vec V.! lastTailIdx

    if x >= lastTailVal
      then do
        writeArray parents i lastTailIdx
        writeArray tails currLen i
        writeArray lenRef 0 (currLen + 1)
      else do
        replaceIdx <- binarySearch tails vec x 0 (currLen - 1)
        writeArray tails replaceIdx i
        when (replaceIdx > 0) $ do
          prevTailIdx <- readArray tails (replaceIdx - 1)
          writeArray parents i prevTailIdx

  finalLen <- readArray lenRef 0
  lastIdx <- readArray tails (finalLen - 1)

  let reconstruct idx acc
        | idx == -1 = return acc
        | otherwise = do
            prev <- readArray parents idx
            reconstruct prev ((vec V.! idx) : acc)

  reconstruct lastIdx []

lgis :: String -> String
lgis input = output
  where
    (_, nums) = case lines input of
      [_, ns] -> ((), map read (words ns) :: [Int])
      _ -> error "Invalid input"
    iRes = solve nums
    dRes = map negate (solve (map negate nums))
    output = unlines [unwords (map show iRes), unwords (map show dRes)]