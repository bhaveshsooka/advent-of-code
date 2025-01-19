module AOC2024.Day10
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.HashMap.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord), neighbors4)
import Util.GridUtils.DirectionVonNeumann (Direction (..))
import Util.GridUtils.Grid (GridInfo, parseGrid)
import Prelude hiding (Left, Right)

part1 :: T.Text -> Int
part1 input = foldr (\ps acc -> if not $ null ps then acc + 1 else acc) 0 paths
  where
    paths = foldr allPathsFold M.empty startEndPairs
    allPathsFold (s, e) = M.insert (s, e) (dfs gridInfo s e)
    startEndPairs = [(c1, c2) | (c1, ch1) <- gridList, (c2, ch2) <- gridList, ch1 == 0 && ch2 == 9]
    gridList = V.toList grid
    gridInfo@(grid, _, _) = parseGrid digitToInt input

part2 :: T.Text -> Int
part2 input = foldr ((+) . length) 0 paths
  where
    paths = foldr allPathsFold M.empty startEndPairs
    allPathsFold (s, e) = M.insert (s, e) (dfs gridInfo s e)
    startEndPairs = [(c1, c2) | (c1, ch1) <- gridList, (c2, ch2) <- gridList, ch1 == 0 && ch2 == 9]
    gridList = V.toList grid
    gridInfo@(grid, _, _) = parseGrid digitToInt input

type TopographyInfo = GridInfo Int

type Path = [(Coord, Direction)]

dfs :: TopographyInfo -> Coord -> Coord -> [Path]
dfs (grid, rows, cols) start end = go S.empty [] start
  where
    go visited path current@(Coord x y)
      | current == end = [path]
      | otherwise = concat $ mapMaybe buildPaths neighbors
      where
        buildPaths (coord, dir)
          | coord `S.member` visited = Nothing
          | otherwise = Just $ go (S.insert current visited) ((coord, dir) : path) coord
        neighbors = filter isValid $ zip (neighbors4 (Coord x y)) [N, W, E, S]
        inBounds (Coord x' y') = x' >= 0 && x' < rows && y' >= 0 && y' < cols
        isValid (coord@(Coord nx ny), _) =
          inBounds coord
            && snd (grid V.! (nx * rows + ny)) - snd (grid V.! (x * rows + y)) == 1
            && coord `S.notMember` visited
