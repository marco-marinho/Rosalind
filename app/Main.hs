module Main (main) where

import Dna (countNucleotides)
import Fib (fib)
import Gc (solveGc)
import Hamm (mutations)
import Revc (reverseComplement)
import Rna (transcribe)
import Iprb (mendelLaw)
import Prot (translate)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (name : _) | Just action <- lookup name dispatch -> action
    [] -> putStrLn "Please provide a problem name."
    _ -> putStrLn "Unknown problem name."

solve :: String -> (String -> String) -> IO ()
solve name f = readFile ("data/" ++ name ++ ".txt") >>= putStrLn . f

dispatch :: [(String, IO ())]
dispatch =
  [ ("dna", solve "dna" countNucleotides),
    ("rna", solve "rna" transcribe),
    ("revc", solve "revc" reverseComplement),
    ("fib", solve "fib" fib),
    ("hamm", solve "hamm" mutations),
    ("gc", solve "gc" solveGc),
    ("iprb", solve "iprb" mendelLaw),
    ("prot", solve "prot" translate)
  ]