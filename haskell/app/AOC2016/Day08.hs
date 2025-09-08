module AOC2016.Day08
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Coord (..))
import Util.GridUtils.Grid ( Grid, showGrid, newGrid )
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = V.foldr (\(_, c) acc -> if c == '#' then acc + 1 else acc) 0 finalGrid
  where
    finalGrid = foldl (\acc op -> processOperation (acc, cols, rows) op) grid $ parseOperations input
    cols = 50
    rows = 6
    grid = newGrid cols rows '.'

part2 :: T.Text -> T.Text
part2 input = showGrid T.singleton (finalGrid, rows)
  where
    finalGrid = foldl (\acc op -> processOperation (acc, cols, rows) op) grid $ parseOperations input
    cols = 50
    rows = 6
    grid = newGrid cols rows '.'

data Operation
  = On Int Int
  | RotateRow Int Int
  | RotateColumn Int Int
  deriving (Show)

processOperation :: (Grid Char, Int, Int) -> Operation -> Grid Char
processOperation (grid, rowLen, _) (On cols rows) = grid V.// newCoords
  where
    newCoords = [(x * rowLen + y, (Coord x y, '#')) | x <- [0 .. rows - 1], y <- [0 .. cols - 1]]
processOperation (grid, rowLen, _) (RotateRow x by) = grid V.// (rotateFn <$> currentCoords)
  where
    rotateFn (Coord x' y, c) = (x' * rowLen + newY y, (Coord x' (newY y), c))
    newY y = (y + by) `mod` rowLen
    currentCoords = V.toList $ V.filter (\(Coord x' _, _) -> x' == x) grid
processOperation (grid, rowLen, colLen) (RotateColumn y by) = grid V.// (rotateFn <$> currentCoords)
  where
    rotateFn (Coord x y', c) = (newX x * rowLen + y', (Coord (newX x) y', c))
    newX x = (x + by) `mod` colLen
    currentCoords = V.toList $ V.filter (\(Coord _ y', _) -> y' == y) grid

parseOperations :: T.Text -> [Operation]
parseOperations input = parseAoCInput input operationsParser "operationsParser"
  where
    numParser = read <$> P.many1 P.digit
    byParser = P.string " by " *> numParser
    onParser = On <$> (P.string "rect " *> numParser <* P.char 'x') <*> numParser
    rotateRowParser = RotateRow <$> (P.string "rotate row y=" *> numParser) <*> byParser
    rotateColumnParser = RotateColumn <$> (P.string "rotate column x=" *> numParser) <*> byParser
    operationParser = P.choice (P.try <$> [onParser, rotateRowParser, rotateColumnParser])
    operationsParser = P.many1 $ operationParser <* P.optional P.newline
