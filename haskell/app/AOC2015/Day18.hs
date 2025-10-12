module AOC2015.Day18
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord), neighbors8)
import Util.GridUtils.Grid (GridInfo, findValByCoord, inBounds, parseGrid, updateAtCoord)

part1 :: T.Text -> Int
part1 input = V.foldr (\(_, v) acc -> if v == '#' then acc + 1 else acc) 0 finalGrid
  where
    n = 100 :: Int
    finalGrid = foldr (\_ g -> updateGrid False (g, cols, rows) <$> g) grid [1 .. n]
    (grid, cols, rows) = parseGrid id input

part2 :: T.Text -> Int
part2 input = V.length $ V.filter ((== '#') . snd) finalGrid
  where
    n = 100 :: Int
    finalGrid = foldr (\_ g -> updateGrid True (g, cols, rows) <$> g) startingGrid [1 .. n]
    startingGrid = foldr (\c g -> updateAtCoord (g, cols, rows) (c, '#')) grid cornerCoords
    cornerCoords = [Coord 0 0, Coord 0 (rows - 1), Coord (cols - 1) 0, Coord (cols - 1) (rows - 1)]
    (grid, cols, rows) = parseGrid id input

updateGrid :: Bool -> GridInfo Char -> (Coord, Char) -> (Coord, Char)
updateGrid keepCorners gi@(_, cols, rows) (c, v) =
  case (keepCorners, v, onNeighborsCount) of
    (True, _, _) | isCorner c -> (c, '#')
    (_, '#', n) | n == 2 || n == 3 -> (c, '#')
    (_, '.', 3) -> (c, '#')
    _ -> (c, '.')
  where
    isCorner c' =
      c' == Coord 0 0
        || c' == Coord 0 (rows - 1)
        || c' == Coord (cols - 1) 0
        || c' == Coord (cols - 1) (rows - 1)
    onNeighborsCount = foldr addIfNeightbourIsOn (0 :: Int) (neighbors8 c)
    addIfNeightbourIsOn c' acc = if inBounds gi c' && findValByCoord gi c' == '#' then acc + 1 else acc