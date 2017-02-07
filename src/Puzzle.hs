{-# LANGUAGE TupleSections #-}

module Puzzle where

import Data.List (intercalate, (\\))
import Data.Maybe
import qualified Data.Vector as V

-- 0 represents not filled
type Row = V.Vector Int
type Puzzle = V.Vector Row

printPuzzle :: Puzzle -> IO ()
printPuzzle p = do
  putStr "+---+---+---+---+---+---+---+---+---+"
  putStrLn $ intercalate "\n+---+---+---+---+---+---+---+---+---+" $ V.toList $ V.map showLine p
  putStrLn "+---+---+---+---+---+---+---+---+---+"
  where
    showLine :: Row -> String
    showLine row = "\n| " ++ intercalate " | " (V.toList $ V.map (\i -> if i == 0 then " " else show i) row) ++ " |"

filterFilled :: Row -> Row
filterFilled = V.filter (> 0)

numbersInRow :: Puzzle -> Int -> Row
numbersInRow p n = filterFilled (p V.! n)

numbersInColumn :: Puzzle -> Int -> Row
numbersInColumn p n = filterFilled $ V.map (\r -> r V.! n) p

square :: Puzzle -> Int -> Int -> V.Vector Int
square p x y = V.concat . V.toList . V.map (V.slice (x * 3) 3) . V.slice (y * 3) 3 $ p

numbersInSquare :: Puzzle -> (Int, Int) -> V.Vector Int
numbersInSquare p (row, column) = (filterFilled $ square p (column `quot` 3) (row `quot` 3))

emptySquares :: Puzzle -> [(Int, Int)]
emptySquares = concat . V.toList . V.imap findEmptyInRow
  where
    findEmptyInRow i = V.toList . V.map (i,) . V.map fst . V.filter ((==) 0 . snd) . V.imap (,)

possible :: Puzzle -> (Int, Int) -> [Int]
possible p (row,column) = (possibleSquare \\ (V.toList $ numbersInRow p row)) \\ (V.toList $ numbersInColumn p column)
  where
    possibleSquare = [1..9] \\ V.toList (numbersInSquare p (row, column))

isSolved :: Puzzle -> Bool
isSolved p =
  let
    positionValid (row, column) =
      let
        number = (p V.! row) V.! column
        existsOne n v = 1 == V.length (V.findIndices ((==) n) v)
        validRow = existsOne number (numbersInRow p row)
        validColumn = existsOne number (numbersInColumn p column)
      in
        validRow && validColumn
    indexes = concat $ map (\n -> zip (repeat n) [0..8]) [0..8]
  in
    (V.all (V.all (/= 0)) p) && all positionValid indexes

isPuzzleSolvable :: Puzzle -> Bool
isPuzzleSolvable p = all ((flip (>)) 0 . length . possible p) $ emptySquares p

substitute :: Puzzle -> (Int, Int) -> Int -> Puzzle
substitute p (row, column) n =
  let
    newRow = (p V.! row) V.// [(column, n)]
  in
    p V.// [(row, newRow)]

solve :: Puzzle -> Maybe Puzzle
solve p =
  if isSolved p
    then Just p
    else Nothing

-- solve :: Puzzle -> Maybe Puzzle
-- solve p =
--   let
--     allPossible = map (\coords -> (coords, possible p coords)) (emptySquares p)
--     try (pos, possible) = map (substitute p pos) possible
--     substitutions = concat $ map try allPossible
--   in
--     if isSolved p
--       then Just p
--       else (find isPuzzleSolvable substitutions) >>= solve
