module AOC2023.Day03
  ( part1,
    part2,
  )
where

import Control.Arrow qualified as BF
import Data.Char (isDigit)
import Data.List (nub)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V

part1 :: T.Text -> Int
part1 input = sum $ enginePart2Num <$> engineParts
  where
    enginePart2Num enginePart = read $ V.foldl (\acc (_, c) -> acc <> [c]) "" enginePart :: Int
    engineParts = filter (isEnginePart (engine, rowLen, colLen)) engineNumbers
    engineNumbers = filter (all (isDigit . snd)) $ V.groupBy groupByFn engine
    groupByFn (Coord x1 _, c1) (Coord x2 _, c2) = isDigit c1 && isDigit c2 && x1 == x2
    (engine, rowLen, colLen) = parseEngine input

part2 :: T.Text -> Int
part2 input = foldr gearRatiosFn 0 starmap
  where
    gearRatiosFn eps acc =
      if length eps == 2
        then acc + S.foldr (\ep acc' -> acc' * enginePart2Num ep) 1 eps
        else acc
    starmap = buildStarmap M.empty (engine, rowLen, colLen) engineParts
    enginePart2Num enginePart = read $ V.foldl (\acc (_, c) -> acc <> [c]) "" enginePart :: Int
    engineParts = filter (isEnginePart (engine, rowLen, colLen)) engineNumbers
    engineNumbers = filter (all (isDigit . snd)) $ V.groupBy groupByFn engine
    groupByFn (Coord x1 _, c1) (Coord x2 _, c2) = isDigit c1 && isDigit c2 && x1 == x2
    (engine, rowLen, colLen) = parseEngine input

data Coord = Coord Int Int deriving (Show, Ord)

type Engine = V.Vector (Coord, Char)

type Starmap = M.Map Coord (S.Set (V.Vector (Coord, Char)))

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x1 y1) == (Coord x2 y2) = x1 == x2 && y1 == y2

buildStarmap :: Starmap -> (Engine, Int, Int) -> [V.Vector (Coord, Char)] -> Starmap
buildStarmap acc _ [] = acc
buildStarmap acc engineInfo (enginePart : engineParts) = buildStarmap (enginePart2Starmap acc engineInfo enginePart) engineInfo engineParts

enginePart2Starmap :: Starmap -> (Engine, Int, Int) -> V.Vector (Coord, Char) -> Starmap
enginePart2Starmap starmap (engine, rowLen, colLen) engineNumber = updateStarmap starmap engineNumber neighbourValues
  where
    neighbourValues = mapMaybe (\(Coord x' y') -> engine V.!? (x' * rowLen + y')) allNeighbors
    allNeighbors = nub $ V.foldl (\acc (coord, _) -> acc <> neighbors coord) [] engineNumber
    neighbors (Coord x y) = filter coordInGrid [Coord (x + x') (y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1]]
    coordInGrid (Coord x' y') = x' >= 0 && x' < rowLen && y' >= 0 && y' < colLen

updateStarmap :: Starmap -> V.Vector (Coord, Char) -> [(Coord, Char)] -> Starmap
updateStarmap acc _ [] = acc
updateStarmap acc engineNumber ((Coord x y, c) : ns) = case c of
  '*' -> updateStarmap (M.insertWith S.union (Coord x y) (S.singleton engineNumber) acc) engineNumber ns
  _ -> updateStarmap acc engineNumber ns

isEnginePart :: (Engine, Int, Int) -> V.Vector (Coord, Char) -> Bool
isEnginePart (engine, rowLen, colLen) engineNumber
  | V.length engineNumber == 0 = False
  | otherwise = anySymbol || isEnginePart (engine, rowLen, colLen) (V.tail engineNumber)
  where
    (Coord x y, _) = V.head engineNumber
    anySymbol = any (\(_, c) -> (not . isDigit) c && c /= '.') neighbourValues
    neighbourValues = mapMaybe (\(Coord x' y') -> engine V.!? (x' * rowLen + y')) neighbors
    neighbors = filter coordInGrid [Coord (x + x') (y + y') | x' <- [-1, 0, 1], y' <- [-1, 0, 1]]
    coordInGrid (Coord x' y') = x' >= 0 && x' < rowLen && y' >= 0 && y' < colLen

parseEngine :: T.Text -> (Engine, Int, Int)
parseEngine input = (grid, rowLen grid, colLen grid)
  where
    grid = go (T.lines input) V.empty 0
    rowLen (g :: Engine) = V.maximum ((\(Coord x _) -> x) . fst <$> g) + 1
    colLen (g :: Engine) = V.maximum ((\(Coord _ y) -> y) . fst <$> g) + 1

    go :: [T.Text] -> Engine -> Int -> Engine
    go [] acc _ = acc
    go (x : xs) acc row = go xs (acc V.++ V.fromList columns) (row + 1)
      where
        columns = BF.first (Coord row) <$> zip [0 ..] (T.unpack x)
