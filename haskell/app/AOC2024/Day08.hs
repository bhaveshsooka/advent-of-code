module AOC2024.Day08
  ( part1,
    part2,
  )
where

import Data.List (nub)
import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord))
import Util.GridUtils.Grid (GridInfo, parseGrid)

part1 :: T.Text -> Int
part1 input = (length . nub) antinodes
  where
    antinodes = concatMap (calcAntinodes gridInfo False [] 1) antennaePairs
    antennaePairs =
      [ (c1, c2)
        | (i1, (c1, ch1)) <- indexedGrid,
          (i2, (c2, ch2)) <- indexedGrid,
          ch1 /= '.' && ch2 /= '.' && ch1 == ch2 && c1 /= c2 && i1 < i2
      ]
    indexedGrid = V.toList $ V.indexed grid
    gridInfo@(grid, _, _) = parseGrid id input

part2 :: T.Text -> Int
part2 input = (length . nub) antinodes
  where
    antinodes = concatMap (calcAntinodes gridInfo True [] rows) antennaePairs
    antennaePairs =
      [ (c1, c2)
        | (i1, (c1, ch1)) <- indexedGrid,
          (i2, (c2, ch2)) <- indexedGrid,
          ch1 /= '.' && ch2 /= '.' && ch1 == ch2 && c1 /= c2 && i1 < i2
      ]
    indexedGrid = V.toList $ V.indexed grid
    gridInfo@(grid, rows, _) = parseGrid id input

type CityMapInfo = GridInfo Char

calcAntinodes :: CityMapInfo -> Bool -> [Coord] -> Int -> (Coord, Coord) -> [Coord]
calcAntinodes gridInfo includePair acc factor (c1@(Coord x1 y1), c2@(Coord x2 y2)) =
  if factor == 0
    then if includePair then c1 : c2 : acc else acc
    else case newAntinodes of
      [] -> calcAntinodes gridInfo includePair acc (factor - 1) (c1, c2)
      antinodes -> calcAntinodes gridInfo includePair (antinodes ++ acc) (factor - 1) (c1, c2)
  where
    inBounds (Coord x y) = x >= 0 && x < rows && y >= 0 && y < cols
    newAntinodes = filter inBounds $ if y1 < y2 then diag1 else diag2
    diag1 = [Coord (x1 - xDiff) (y1 - yDiff), Coord (x2 + xDiff) (y2 + yDiff)]
    diag2 = [Coord (x2 + xDiff) (y2 - yDiff), Coord (x1 - xDiff) (y1 + yDiff)]
    xDiff = factor * abs (x2 - x1)
    yDiff = factor * abs (y2 - y1)
    (_, rows, cols) = gridInfo
