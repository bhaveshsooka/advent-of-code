module AOC2023.Day03
  ( part1,
    part2,
  )
where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord))
import Util.GridUtils.Grid (GridInfo, parseGrid)

part1 :: T.Text -> Int
part1 input = foldr (\ep acc -> acc + (read . V.toList $ snd <$> ep)) 0 engineParts
  where
    engineParts = filter (isEnginePart gridInfo) engineNumbers
    engineNumbers = filter (all (isDigit . snd)) $ V.groupBy engineNumberGroupBy engine
    engineNumberGroupBy (Coord x1 _, c1) (Coord x2 _, c2) = isDigit c1 && isDigit c2 && x1 == x2
    gridInfo@(engine, _, _) = parseGrid id input

part2 :: T.Text -> Int
part2 input = foldr gearRatiosFn 0 starmap
  where
    gearRatiosFn eps acc =
      if length eps == 2
        then acc + foldr (\ep acc' -> acc' * (read . V.toList $ snd <$> ep)) 1 eps
        else acc
    starmap = foldl (updateStarmapWithEnginePart gridInfo) M.empty engineParts
    engineParts = filter (isEnginePart gridInfo) engineNumbers
    engineNumbers = filter (all (isDigit . snd)) $ V.groupBy engineNumberGroupBy engine
    engineNumberGroupBy (Coord x1 _, c1) (Coord x2 _, c2) = isDigit c1 && isDigit c2 && x1 == x2
    gridInfo@(engine, _, _) = parseGrid id input

type EngineInfo = GridInfo Char

type GridElem = (Coord, Char)

type EnginePart = V.Vector GridElem

type Starmap = M.Map Coord (S.Set EnginePart)

updateStarmapWithEnginePart :: EngineInfo -> Starmap -> EnginePart -> Starmap
updateStarmapWithEnginePart gridInfo starmap engineNumber = foldl updateStarmapFold starmap neighbourValues
  where
    updateStarmapFold acc (Coord x y, c) =
      if c == '*'
        then M.insertWith S.union (Coord x y) (S.singleton engineNumber) acc
        else acc
    neighbourValues = mapMaybe (\(Coord x' y') -> engine V.!? (x' * rowLen + y')) allNeighbors
    allNeighbors = nub $ V.foldl (\acc (coord, _) -> acc <> neighbors coord) [] engineNumber
    neighbors (Coord x y) = filter coordInGrid [Coord (x + x') (y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1]]
    coordInGrid (Coord x y) = x >= 0 && x < rowLen && y >= 0 && y < colLen
    (engine, rowLen, colLen) = gridInfo

isEnginePart :: EngineInfo -> EnginePart -> Bool
isEnginePart gridInfo = foldr (\(Coord x y, _) acc -> acc || anyNeighbourSymbol (Coord x y)) False
  where
    anyNeighbourSymbol coord = any (\(_, c) -> (not . isDigit) c && c /= '.') $ neighbourValues coord
    neighbourValues (Coord x y) = mapMaybe (\(Coord x' y') -> engine V.!? (x' * rowLen + y')) $ neighbors (Coord x y)
    neighbors (Coord x y) = filter coordInGrid [Coord (x + x') (y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1]]
    coordInGrid (Coord x y) = x >= 0 && x < rowLen && y >= 0 && y < colLen
    (engine, rowLen, colLen) = gridInfo
