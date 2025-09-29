module AOC2015.Day17
  ( part1,
    part2,
  )
where

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = length $ filter ((== 150) . sum) $ subsets $ parseBuckets input

part2 :: T.Text -> Int
part2 input = length $ filter ((== minLen) . length) validBuckets
  where
    validBuckets = filter ((== 150) . sum) $ subsets $ parseBuckets input
    minLen = length $ minimumBy (compare `on` length) validBuckets

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x : xs) = subsets xs ++ map (x :) (subsets xs)

parseBuckets :: T.Text -> [Int]
parseBuckets input = parseAoCInput input bucketsParser "bucketsParser"
  where
    numParser = read <$> P.many1 P.digit
    bucketsParser = P.many1 $ numParser <* P.optional P.newline