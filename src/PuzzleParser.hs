module PuzzleParser (parse) where

import Data.Either
import qualified Data.Vector as V

import Text.Parsec hiding (parse)
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Puzzle

separator :: Parser ()
separator = string "+---+---+---+---+---+---+---+---+---+" >> endOfLine >> return ()

number :: Parser Int
number = do
  char ' '
  d <- digit
  char ' '
  char '|'
  return (read [d] :: Int)

row :: Parser Row
row = do
  char '|'
  numbers <- count 9 (try number <|> (string "   |" >> return 0))
  endOfLine
  separator
  return $ V.fromList numbers

puzzle :: Parser Puzzle
puzzle = do
  separator
  rows <- count 9 row
  eof
  return $ V.fromList rows

parse :: String -> IO (Either String Puzzle)
parse file = do
  result <- parseFromFile puzzle file
  case result of
    Left err -> return . Left . show $ err
    Right puzzle -> return . Right $ puzzle
