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

part2 :: T.Text -> String
part2 _input = "Not there yet"

atleastThreeVowels :: T.Text -> Bool
atleastThreeVowels = (>= 3) . T.length . T.filter (`elem` "aeiou")

containsDoubleLetter :: T.Text -> Bool
containsDoubleLetter = any ((> 1) . T.length) . T.group

doesNotContainBadStrings :: T.Text -> Bool
doesNotContainBadStrings x = not $ any ((`T.isInfixOf` x) . T.pack) ["ab", "cd", "pq", "xy"]
