module AOC2015.Day01 (
  parts,
) where

import Data.Text qualified as T
import Util.AOCHelpers (Part (Part), Parts)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = sum $ floorDirections input

part2 :: T.Text -> Int
part2 input = (+ 1) $ length $ scanlWhile $ floorDirections input
 where
  scanlWhile = takeWhile (/= (-1)) . scanl1 (+)

calcNextFloor :: Char -> Int
calcNextFloor c
  | c == '(' = 1
  | c == ')' = -1
  | otherwise = 0

floorDirections :: T.Text -> [Int]
floorDirections input = calcNextFloor <$> T.unpack input
