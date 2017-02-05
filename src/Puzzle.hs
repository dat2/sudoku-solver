module Puzzle where

import Debug.Trace

import Data.Maybe
import Data.List

-- simple for now
type Row = [Maybe Integer]
type Puzzle = [Row]

example :: Puzzle
example = [
    [ Just 5, Just 3, Nothing, Nothing, Just 7, Nothing, Nothing, Nothing, Nothing ]
  , [ Just 6, Nothing, Nothing, Just 1, Just 9, Just 5, Nothing, Nothing, Nothing ]
  , [ Nothing, Just 9, Just 8, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing ]

  , [ Just 8, Nothing, Nothing, Nothing, Just 6, Nothing, Nothing, Nothing, Just 3 ]
  , [ Just 4, Nothing, Nothing, Just 8, Nothing, Just 3, Nothing, Nothing, Just 1 ]
  , [ Just 7, Nothing, Nothing, Nothing, Just 2, Nothing, Nothing, Nothing, Just 6 ]

  , [ Nothing, Just 6, Nothing, Nothing, Nothing, Nothing, Just 2, Just 8, Nothing ]
  , [ Nothing, Nothing, Nothing, Just 4, Just 1, Just 9, Nothing, Nothing, Just 5 ]
  , [ Nothing, Nothing, Nothing, Nothing, Just 8, Nothing, Nothing, Just 7, Just 9 ] ]

solved :: Puzzle
solved = [
    [ Just 5, Just 3, Just 4, Just 6, Just 7, Just 8, Just 9, Just 1, Just 2 ]
  , [ Just 6, Just 7, Just 2, Just 1, Just 9, Just 5, Just 3, Just 4, Just 8 ]
  , [ Just 1, Just 9, Just 8, Just 3, Just 4, Just 2, Just 5, Just 6, Just 7 ]

  , [ Just 8, Just 5, Just 9, Just 7, Just 6, Just 1, Just 4, Just 2, Just 3 ]
  , [ Just 4, Just 2, Just 6, Just 8, Just 5, Just 3, Just 7, Just 9, Just 1 ]
  , [ Just 7, Just 1, Just 3, Just 9, Just 2, Just 4, Just 8, Just 5, Just 6 ]

  , [ Just 9, Just 6, Just 1, Just 5, Just 3, Just 7, Just 2, Just 8, Just 4 ]
  , [ Just 2, Just 8, Just 7, Just 4, Just 1, Just 9, Just 6, Just 3, Just 5 ]
  , [ Just 3, Just 4, Just 5, Just 2, Just 8, Just 6, Just 1, Just 7, Just 9 ] ]

printPuzzle :: Puzzle -> IO ()
printPuzzle p = do
  putStr "+---+---+---+---+---+---+---+---+---+"
  putStrLn $ intercalate "\n+---+---+---+---+---+---+---+---+---+" $ map showLine p
  putStrLn "+---+---+---+---+---+---+---+---+---+"
  where
    showLine :: Row -> String
    showLine row = "\n| " ++ intercalate " | " (map (maybe " " show) row) ++ " |"

filterFilled :: [Maybe Integer] -> [Integer]
filterFilled = map fromJust . filter isJust

numbersInRow :: Puzzle -> Int -> [Integer]
numbersInRow p n = filterFilled (p !! n)

numbersInColumn :: Puzzle -> Int -> [Integer]
numbersInColumn p n = filterFilled $ map (\r -> r !! n) p

-- | index, length
sublist :: Int -> Int -> [a] -> [a]
sublist i n = take n . drop i

square :: Puzzle -> Int -> Int -> Row
square p x y = concat . map (sublist (x * 3) 3) . sublist (y * 3) 3 $ p

numbersInSquare :: Puzzle -> (Int, Int) -> [Integer]
numbersInSquare p (row, column) = (filterFilled $ square p (column `quot` 3) (row `quot` 3))

emptySquares :: Puzzle -> [(Int,Int)]
emptySquares = concat . map (\(row, columns) -> zip (repeat row) columns) . zip [0..8] . map findEmptyInRow
  where
    findEmptyInRow = map fst . filter (isNothing . snd) . zip [0..8]

possible :: Puzzle -> (Int, Int) -> [Integer]
possible p (row,column) = (possibleSquare \\ (numbersInRow p row)) \\ (numbersInColumn p column)
  where
    possibleSquare = [1..9] \\ numbersInSquare p (row, column)

isSolved :: Puzzle -> Bool
isSolved p =
  let
    positionValid (row, column) =
      let
        number = fromJust $ (p !! row) !! column
        existsOne n list = 1 == length (filter (== n) list)
        validRow = existsOne number (numbersInRow p row)
        validColumn = existsOne number (numbersInColumn p column)
      in
        validRow && validColumn
    indexes = concat $ map (\n -> zip (repeat n) [0..8]) [0..8]
  in
    (all (all isJust)) p && all positionValid indexes

isPuzzleSolvable :: Puzzle -> Bool
isPuzzleSolvable p = all ((flip (>)) 0 . length . possible p) $ emptySquares p

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index element list =
    let
      (before, elementAndAfter) = splitAt index list
      after = tail elementAndAfter
    in
      before ++ [element] ++ after

substitute :: Puzzle -> (Int, Int) -> Integer -> Puzzle
substitute p (row, column) n =
  let
    newRow = replaceAt column (Just n) (p !! row)
  in
    replaceAt row newRow p

solve :: Puzzle -> Maybe Puzzle
solve p =
  let
    allPossible = map (\coords -> (coords, possible p coords)) (emptySquares p)
    try (pos, possible) = map (substitute p pos) possible
    substitutions = concat $ map try allPossible
  in
    if isSolved p
      then Just p
      else (find isPuzzleSolvable substitutions) >>= solve
