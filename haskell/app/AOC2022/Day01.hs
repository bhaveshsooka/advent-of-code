module AOC2022.Day01
  ( part1,
    part2,
  )
where

import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = maximum $ sum <$> parseCalories input

part2 :: T.Text -> Int
part2 input = sum $ take 3 $ sortBy (comparing Down) (sum <$> parseCalories input)

parseCalories :: T.Text -> [[Int]]
parseCalories input = go [] [] $ T.lines input
  where
    go :: [[Int]] -> [Int] -> [T.Text] -> [[Int]]
    go acc accumList [] = acc ++ [accumList]
    go acc accumList (x : xs) =
      if T.unpack x == ""
        then go (acc ++ [accumList]) [] xs
        else go acc (accumList ++ [read $ T.unpack x]) xs
