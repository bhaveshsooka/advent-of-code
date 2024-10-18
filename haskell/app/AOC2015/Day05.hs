module AOC2015.Day05 (
  parts,
) where

import Data.Text qualified as T
import Util.AOCHelpers (Part (Part), Parts)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = length $ filter predicate inputLines
 where
  inputLines = T.lines input
  predicate = (\l -> atleastThreeVowels l && containsDoubleLetter l && doesNotContainBadStrings l)

part2 :: T.Text -> String
part2 _input = "Not there yet"

atleastThreeVowels :: T.Text -> Bool
atleastThreeVowels = (>= 3) . T.length . T.filter (`elem` "aeiou")

containsDoubleLetter :: T.Text -> Bool
containsDoubleLetter = any ((> 1) . T.length) . T.group

doesNotContainBadStrings :: T.Text -> Bool
doesNotContainBadStrings x = not $ any (`T.isInfixOf` x) (T.pack <$> ["ab", "cd", "pq", "xy"])
