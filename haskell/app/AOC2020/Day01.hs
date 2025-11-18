module AOC2020.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum intPairs
  where
    intPairs :: [Int] = [x * y | (i1, x) <- intList, (i2, y) <- intList, i1 < i2, x + y == 2020]
    intList :: [(Int, Int)] = zipWith (\a b -> (a, read b)) [1 ..] $ lines (T.unpack input)

part2 :: T.Text -> Int
part2 input = sum intPairs
  where
    limit :: Int = 2020
    intPairs :: [Int] =
      [ x * y * z
        | (i2, y) <- intList,
          (i1, x) <- intList,
          i1 < i2,
          (i3, z) <- intList,
          i2 < i3,
          x + y + z == limit
      ]
    intList :: [(Int, Int)] = zipWith (\a b -> (a, read b)) [1 ..] $ lines (T.unpack input)
