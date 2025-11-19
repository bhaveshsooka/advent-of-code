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
    exactly2s = filter (hasExactly 2) strings
    exactly3s = filter (hasExactly 3) strings
    hasExactly n s = any ((== n) . length) (group . sort $ s)
    strings = lines $ T.unpack input

part2 :: T.Text -> String
part2 input = findAlmostTwins zippedStrPairs
  where
    zippedStrPairs = [zip x y | (i1, x) <- indexedStrings, (i2, y) <- indexedStrings, i1 < i2]
    indexedStrings = zip [1 :: Int ..] (lines $ T.unpack input)

findAlmostTwins :: [[(Char, Char)]] -> String
findAlmostTwins [] = "error"
findAlmostTwins (x : xs) =
  if countDifferingChars x == 1
    then uncurry intersect . unzip $ x
    else findAlmostTwins xs
  where
    countDifferingChars = foldr (\(a, b) acc -> if a == b then acc else acc + 1) (0 :: Int)
