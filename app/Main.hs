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
import Lexf (lexf)
import Lgis (lgis)
import Lia (lia)
import Mprt (mprt)
import Mrna (mrna)
import Orf (orf)
import Perm (perm)
import Prot (translate)
import Prtm (prtm)
import Revc (reverseComplement)
import Revp (revp)
import Rna (transcribe)
import Splc (splc)
import Subs (motif)
import System.Environment (getArgs)
import System.Process (readProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (name : _) | Just action <- lookup name dispatch -> action
    [] -> putStrLn "Please provide a problem name."
    _ -> putStrLn "Unknown problem name."

solve :: String -> (String -> String) -> IO ()
solve name f = readFile ("data/" ++ name ++ ".txt") >>= putStrLn . f

solveMprt :: String -> IO ()
solveMprt name = do
  input <- readFile ("data/" ++ name ++ ".txt")
  let inputs_clean = map (takeWhile (/= '_')) (lines input)
      request = map (\s -> "https://www.uniprot.org/uniprot/" ++ s ++ ".fasta") inputs_clean
  responses <- mapM (\r -> filter (/= '\n') . dropWhile (/= '\n') <$> readProcess "curl" ["-s", "-L", r] "") request
  let res = mprt (zip (lines input) responses)
  putStrLn res

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
    ("iev", solve "iev" iev),
    ("prtm", solve "prtm" prtm),
    ("mrna", solve "mrna" mrna),
    ("lia", solve "lia" lia),
    ("splc", solve "splc" splc),
    ("mprt", solveMprt "mprt"),
    ("perm", solve "perm" perm),
    ("orf", solve "orf" orf),
    ("revp", solve "revp" revp),
    ("lexf", solve "lexf" lexf),
    ("lgis", solve "lgis" lgis)
  ]