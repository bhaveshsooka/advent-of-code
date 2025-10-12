module AOC2015.Day20
  ( part1,
    part2,
  )
where

import Data.List (findIndex)
import Data.Text qualified as T
import Util.MathUtils (factors)

part1 :: T.Text -> Maybe Int
part1 input = findIndex (>= presentsTarget) . map presents $ [0 ..]
  where
    presents = sum . factors
    presentsTarget :: Int = (read . T.unpack $ input) `div` 10

part2 :: T.Text -> Maybe Int
part2 input = findIndex (>= presentsTarget) . map presents $ [0 ..]
  where
    presents n = (11 *) . sum . filter (> (n - 1) `div` 50) . factors $ n
    presentsTarget :: Int = read . T.unpack $ input
