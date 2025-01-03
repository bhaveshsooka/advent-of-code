{-# LANGUAGE OverloadedStrings #-}

module AOC2024.Day04
  ( part1,
    part2,
  )
where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Text qualified as T
import Prelude hiding (Left, Right)

part1 :: T.Text -> Int
part1 input = countXmas' (fst <$> xs) Up grid 0
  where
    grid = parseGrid input
    xs = filter (\(Coord _ _, val) -> val == 'X') grid

part2 :: T.Text -> Int
part2 input = length $ filter is3x3GridValid (build3x3Grid grid <$> as)
  where
    is3x3GridValid str = str `elem` valid3x3Grids
    grid = parseGrid input
    as = (\(Coord x y, _) -> Coord x y) <$> filter (\(Coord x y, val) -> val == 'A' && x >= 1 && y >= 1) grid
    valid3x3Grids = ["M_M_A_S_S", "M_S_A_M_S", "S_M_A_S_M", "S_S_A_M_M"]

data Coord = Coord Int Int deriving (Show)

type Elem = (Coord, Char)

type Grid = [Elem]

data Direction
  = Up
  | Right
  | Down
  | Left
  | DiagonalUpRight
  | DiagonalDownRight
  | DiagonalDownLeft
  | DiagonalUpLeft
  deriving (Show)

data Path
  = Path [Coord]
  | NoPath
  deriving (Show)

build3x3Grid :: Grid -> Coord -> String
build3x3Grid grid (Coord x y) = strPath' grid "" deltas
  where
    deltas = [Coord x' y' | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]

strPath' :: Grid -> String -> [Coord] -> String
strPath' _ acc [] = (\(i :: Int, e) -> if even i then e else '_') <$> zip [0 ..] acc
strPath' grid acc (p : ps) =
  case getElem p grid of
    Just (_, val) -> strPath' grid (acc ++ [val]) ps
    Nothing -> strPath' grid acc ps

countXmas' :: [Coord] -> Direction -> Grid -> Int -> Int
countXmas' [] _ _ acc = acc
countXmas' (x : xs) dir grid acc = countXmas' xs dir grid (acc + length (isXmas x))
  where
    isXmas cell = filter (== "XMAS") $ paths cell
    paths cell = strPath grid "" <$> getPathForAllDirections cell grid

strPath :: Grid -> String -> Path -> String
strPath _ _ NoPath = ""
strPath _ acc (Path []) = acc
strPath grid acc (Path (p : ps)) =
  case getElem p grid of
    Just (_, val) -> strPath grid (acc ++ [val]) (Path ps)
    Nothing -> strPath grid acc (Path ps)

getPathForAllDirections :: Coord -> Grid -> [Path]
getPathForAllDirections cell grid = [getPathForDirection cell dir grid | dir <- dirs]
  where
    dirs =
      [ Up,
        Right,
        Down,
        Left,
        DiagonalUpRight,
        DiagonalDownRight,
        DiagonalDownLeft,
        DiagonalUpLeft
      ]

getPathForDirection :: Coord -> Direction -> Grid -> Path
getPathForDirection c dir grid =
  if validatePath (path c) grid
    then Path (path c)
    else NoPath
  where
    validatePath p g = all (isJust . (`getElem` g)) p
    path cell = getPath cell []
    getPath cell acc
      | length acc == 4 = acc
      | otherwise = getPath (applyDelta cell (getDelta dir)) (acc ++ [cell])

getElem :: Coord -> Grid -> Maybe Elem
getElem (Coord x y) = find (\(Coord x' y', _) -> x == x' && y == y')

getDelta :: Direction -> (Int, Int)
getDelta Up = (-1, 0)
getDelta Down = (1, 0)
getDelta Left = (0, -1)
getDelta Right = (0, 1)
getDelta DiagonalUpRight = (-1, 1)
getDelta DiagonalUpLeft = (-1, -1)
getDelta DiagonalDownRight = (1, 1)
getDelta DiagonalDownLeft = (1, -1)

applyDelta :: Coord -> (Int, Int) -> Coord
applyDelta (Coord x y) (dx, dy) = Coord (x + dx) (y + dy)

parseGrid :: T.Text -> Grid
parseGrid input = coordParser <$> concatMap unnestRow getRows
  where
    coordParser (row, col, val) = (Coord row col, val)
    rowsHelper = zip (T.lines input) [0 ..]
    colsHelper row = zip (T.unpack row) [0 ..]
    getRows = (\(row, rowIdx) -> (rowIdx, colsHelper row)) <$> rowsHelper
    unnestRow (rowIdx, cols) = (\(val, colIdx) -> (rowIdx, colIdx, val)) <$> cols
