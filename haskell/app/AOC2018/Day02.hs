module AOC2018.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.List (sort, group)

part1 :: T.Text -> Int
part1 input = length exactly2s * length exactly3s
  where
    exactly2s = filter (not . null) $ filter (==2) <$> groupLengths
    exactly3s = filter (not . null) $ filter (==3) <$> groupLengths
    groupLengths = (length <$>) . group . sort <$> strings
    strings = lines $ T.unpack input

part2 :: T.Text -> Int
part2 input = 0
