module AOC2025.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

-- part1 :: T.Text -> Int
part1 input = sum $ concatMap countInvalids ranges
  where
    rangeStrings = T.splitOn (T.pack ",") input
    ranges :: [(Integer, Integer)] =
      map
        ( \s ->
            ( read . T.unpack $ T.splitOn (T.pack "-") s !! 0,
              read . T.unpack $ T.splitOn (T.pack "-") s !! 1
            )
        )
        rangeStrings

part2 :: T.Text -> Int
part2 input = 0

countInvalids :: (Integer, Integer) -> [Integer]
countInvalids (startN, endN) = filter isInvalid explodedRange
  where
    explodedRange = [startN .. endN]

isInvalid :: Integer -> Bool
isInvalid n = take l s == drop l s
  where
    l = length s `div` 2
    s = show n
