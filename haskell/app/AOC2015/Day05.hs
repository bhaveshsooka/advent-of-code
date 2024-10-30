module AOC2015.Day05 (
  part1,
  part2,
) where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ filter predicate inputLines
 where
  inputLines = T.lines input
  predicate l = atleastThreeVowels l && containsDoubleLetter l && doesNotContainBadStrings l
  atleastThreeVowels = (>= 3) . T.length . T.filter (`elem` "aeiou")
  containsDoubleLetter = any ((> 1) . T.length) . T.group
  doesNotContainBadStrings x = not $ any ((`T.isInfixOf` x) . T.pack) ["ab", "cd", "pq", "xy"]

part2 :: T.Text -> Int
part2 input = length $ filter predicate inputLines
 where
  inputLines = T.lines input
  predicate l = hasRepeatedPair (T.unpack l) && hasRepeatingLetterWithGap (T.unpack l)
  hasRepeatedPair s = any (\i -> take 2 (drop i s) `elem` pairs (i + 2)) [0 .. length s - 2]
    where
      pairs start = [take 2 (drop j s) | j <- [start .. length s - 2]]
  hasRepeatingLetterWithGap s = any (\i -> s !! i == s !! (i + 2)) [0 .. length s - 3]
