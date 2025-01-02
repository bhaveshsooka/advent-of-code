module Util.GridHelpers
  ( Coord (..),
    Grid,
    parseGrid,
    newGrid,
    showGrid,
  )
where

import Control.Arrow qualified as BF
import Data.Vector qualified as V

data Coord = Coord Int Int deriving (Show)

type Grid = V.Vector (Coord, Char)

showGrid :: (Grid, Int) -> String
showGrid (grid, rowLen) = foldl foldFn "" rows
  where
    foldFn acc row = acc <> xx row <> "\n"
    xx row = snd <$> V.toList (V.filter (\(Coord x _, _) -> x == row) grid)
    rows = [0 .. rowLen - 1]

newGrid :: Int -> Int -> Char -> Grid
newGrid rowLen colLen char = V.fromList $ [(Coord x y, char) | x <- [0 .. colLen - 1], y <- [0 .. rowLen - 1]]

parseGrid :: forall a. (Show a) => a -> (Grid, Int, Int)
parseGrid input = (grid, rowLen grid, colLen grid)
  where
    grid = go V.empty 0 (lines $ show input)
    rowLen (g :: Grid) = V.maximum ((\(Coord x _) -> x) . fst <$> g) + 1
    colLen (g :: Grid) = V.maximum ((\(Coord _ y) -> y) . fst <$> g) + 1

    go :: Grid -> Int -> [String] -> Grid
    go acc _ [] = acc
    go acc row (x : xs) = go (acc V.++ V.fromList columns) (row + 1) xs
      where
        columns = BF.first (Coord row) <$> zip [0 ..] x
