module Main where

import Puzzle

main :: IO ()
main = do
  printPuzzle example
  putStrLn ""

  maybe (putStrLn "Failed to solve.") printPuzzle (solve example)
