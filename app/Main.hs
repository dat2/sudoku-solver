module Main where

import Data.Maybe
import System.Environment

import Puzzle
import PuzzleParser

process :: Puzzle -> IO ()
process p = do
  printPuzzle p
  let maybeSolved = solve p
  maybe (putStrLn "Failed to solve.") (\p -> putStrLn "Solved." >> printPuzzle p) maybeSolved

main :: IO ()
main = do
  [file] <- getArgs

  puzzle <- parse file
  case puzzle of
    Left err -> print err
    Right puzzle -> process puzzle
