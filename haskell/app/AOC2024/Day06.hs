module AOC2024.Day06
  ( part1,
    part2,
  )
where

import Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import Data.Bits (Bits (shiftL, (.|.)))
import Data.IntSet qualified as IS
import Data.Text qualified as T
import Util.GridUtils.Coord (Coord)
import Util.GridUtils.DirectionVonNeumann (Direction (..), applyDirection, turnRight)
import Util.GridUtils.Grid qualified as AOCGrid

part1 :: T.Text -> Int
part1 input = length . snd $ findVisited gridInfo (start, N)
  where
    gridInfo = AOCGrid.parseGrid id input
    start = AOCGrid.findCoordByVal gridInfo '^'

part2 :: T.Text -> Int
part2 input = parCountBy 8 (fst . loopIfBlocked) (drop 1 originalPath)
  where
    gridInfo@(_, rows, cols) = AOCGrid.parseGrid id input
    start = AOCGrid.findCoordByVal gridInfo '^'
    originalPath = snd $ findVisited gridInfo (start, N)
    loopIfBlocked (c, _) = findVisited (genNewGrid c, rows, cols) (start, N)
    genNewGrid c = AOCGrid.updateAtCoord gridInfo (c, '#')
    parCountBy chunk p = sum . withStrategy (parListChunk chunk rseq) . map (fromEnum . p)

type FloorPlanInfo = AOCGrid.GridInfo Char

type PathElem = (Coord, Direction)

type Path = [PathElem]

type Visited = IS.IntSet

type IsLoop = Bool

findVisited :: FloorPlanInfo -> PathElem -> (IsLoop, Path)
findVisited gi = go IS.empty []
  where
    stateKey i d = (i `shiftL` 2) .|. fromEnum d
    cellSeen s i = any ((`IS.member` s) . stateKey i) [N, E, S, W]

    go :: Visited -> Path -> PathElem -> (IsLoop, Path)
    go firstOnly v (c, d)
      | not (AOCGrid.inBounds gi next) = (False, v')
      | AOCGrid.findValByCoord gi next == '#' = go firstOnly v (c, turnRight d)
      | cellSeen firstOnly i =
          if IS.member (stateKey i d) firstOnly
            then (True, v')
            else go firstOnly v (next, d)
      | otherwise = go (IS.insert (stateKey i d) firstOnly) v' (next, d)
      where
        next = applyDirection d c
        i = AOCGrid.findIdxByCoord gi c
        v' = (c, d) : v
