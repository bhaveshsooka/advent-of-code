module AOC2015.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum $ parseFloorDirections input

part2 :: T.Text -> Int
part2 input = (+ 1) . length $ scanlUntil $ parseFloorDirections input
  where
    scanlUntil = takeWhile (/= (-1)) . scanl1 (+)

parseFloorDirections :: T.Text -> [Int]
parseFloorDirections = T.foldr (\c acc -> if c == '(' then 1 : acc else (-1) : acc) []
