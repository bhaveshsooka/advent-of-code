module AOC2018.Day02
  ( part1,
    part2,
  )
where

import Data.List (group, intersect, sort)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length exactly2s * length exactly3s
  where
    exactly2s = filter (not . null) $ filter (== 2) <$> groupLengths
    exactly3s = filter (not . null) $ filter (== 3) <$> groupLengths
    groupLengths = (length <$>) . group . sort <$> strings
    strings = lines $ T.unpack input

part2 :: T.Text -> String
part2 input =
  case almostTwins of
    [] -> "error"
    (x : _) -> x
  where
    almostTwins = uncurry intersect <$> filter (\(a, b) -> countDifferingChars a b (0 :: Int) == 1) strPairs
    strPairs = [(x, y) | (i1, x) <- strings, (i2, y) <- strings, i1 < i2]
    strings = zip [1 :: Int ..] (lines $ T.unpack input)

countDifferingChars :: (Eq a, Num t) => [a] -> [a] -> t -> t
countDifferingChars [] _ acc = acc
countDifferingChars _ [] acc = acc
countDifferingChars (x : xs) (y : ys) acc =
  if x == y
    then countDifferingChars xs ys acc
    else countDifferingChars xs ys (acc + 1)
