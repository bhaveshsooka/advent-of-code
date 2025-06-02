module AOC2024.Day06
  ( part1,
    part2,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord))
import Util.GridUtils.DirectionVonNeumann (Direction (..), turnRight)
import Util.GridUtils.Grid (GridInfo, parseGrid)

part1 :: T.Text -> Int
part1 input = length . snd $ findVisited gridInfo N start V.empty
  where
    start = fst $ grid V.! fromMaybe (-1) (V.findIndex ((== '^') . snd) grid)
    gridInfo@(grid, _, _) = parseGrid id input

part2 :: T.Text -> Int
part2 input = foldr countLoopsFold 0 (V.tail originalPath)
  where
    countLoopsFold (c, _) acc =
      if fst (findVisited (changeOneValue c, rows, cols) N start V.empty)
        then acc + 1
        else acc
    originalPath = snd $ findVisited gridInfo N start V.empty
    start = fst $ grid V.! fromMaybe (-1) (V.findIndex ((== '^') . snd) grid)
    changeOneValue c@(Coord x y) = grid V.// [(x * rows + y, (c, '#'))]
    gridInfo@(grid, rows, cols) = parseGrid id input

type FloorPlanInfo = GridInfo Char

type Visited = V.Vector (Coord, Direction)

type IsLoop = Bool

findVisited :: FloorPlanInfo -> Direction -> Coord -> Visited -> (IsLoop, Visited)
findVisited gridInfo@(grid, rows, cols) dir c visited =
  if not $ inBounds newCoord
    then (False, newVisited)
    else case grid V.!? (nx * rows + ny) of
      Just (_, '#') -> findVisited gridInfo (turnRight dir) c visited
      Just _ ->
        if any ((== c) . fst) visited
          then
            if (c, dir) `elem` visited
              then (True, newVisited)
              else findVisited gridInfo dir newCoord visited
          else findVisited gridInfo dir newCoord newVisited
      Nothing -> (False, newVisited)
  where
    newCoord@(Coord nx ny) = applyDelta dir c
    inBounds (Coord x y) = x >= 0 && x < rows && y >= 0 && y < cols
    newVisited = visited V.++ V.singleton (c, dir)

applyDelta :: Direction -> Coord -> Coord
applyDelta N (Coord x y) = Coord (x - 1) y
applyDelta S (Coord x y) = Coord (x + 1) y
applyDelta W (Coord x y) = Coord x (y - 1)
applyDelta E (Coord x y) = Coord x (y + 1)
