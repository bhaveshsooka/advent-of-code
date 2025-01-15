module AOC2023.Day01
  ( part1,
    part2,
  )
where

import Data.Char (isDigit)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum numbersList
  where
    numbersList = read . extractFirstAndLast <$> T.lines input

part2 :: T.Text -> Int
part2 input = sum numbersList
  where
    numbersList = read . extractFirstAndLast . replaceWordsWithDigits <$> T.lines input

replaceWordsWithDigits :: T.Text -> T.Text
replaceWordsWithDigits str = foldr (\(old, new) s -> T.replace old new s) str replacements
  where
    replacements =
      [ (T.pack "one", T.pack "o1e"),
        (T.pack "two", T.pack "t2o"),
        (T.pack "three", T.pack "t3e"),
        (T.pack "four", T.pack "f4r"),
        (T.pack "five", T.pack "f5e"),
        (T.pack "six", T.pack "s6x"),
        (T.pack "seven", T.pack "s7n"),
        (T.pack "eight", T.pack "e8t"),
        (T.pack "nine", T.pack "n9e")
      ]

extractFirstAndLast :: T.Text -> String
extractFirstAndLast str = [T.head s, T.last s]
  where
    s = T.filter isDigit str
