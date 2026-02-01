module Main (main) where

import Cons (consensus)
import Dna (countNucleotides)
import Fib (fib)
import Fibd (fibd)
import Gc (solveGc)
import Grph (overlap)
import Hamm (mutations)
import Iev (iev)
import Iprb (mendelLaw)
import Lcsm (lcsm)
import Prot (translate)
import Revc (reverseComplement)
import Rna (transcribe)
import Subs (motif)
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
    ("prot", solve "prot" translate),
    ("subs", solve "subs" motif),
    ("fibd", solve "fibd" fibd),
    ("cons", solve "cons" consensus),
    ("grph", solve "grph" overlap),
    ("lcsm", solve "lcsm" lcsm),
    ("iev", solve "iev" iev)
  ]