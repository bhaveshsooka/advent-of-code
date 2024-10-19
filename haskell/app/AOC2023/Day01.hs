{-# LANGUAGE OverloadedStrings #-}

module AOC2023.Day01 (
  parts,
) where

import Data.Text qualified as T
import Util.AOCHelpers (Part (Part), Parts)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = sum $ read <$> list
 where
  list = extractFirstAndLast <$> numeric_string <$> (T.lines input)

part2 :: T.Text -> Int
part2 input = sum $ read <$> list
 where
  list = extractFirstAndLast <$> numeric_string <$> replace_words_with_digits <$> (T.lines input)

replace_words_with_digits :: T.Text -> T.Text
replace_words_with_digits str =
  replace "one" "o1e" $
    replace "two" "t2o" $
      replace "three" "t3e" $
        replace "four" "f4r" $
          replace "five" "f5e" $
            replace "six" "s6x" $
              replace "seven" "s7n" $
                replace "eight" "e8t" $
                  replace "nine" "n9e" str

-- Helper function to replace all instances of a substring
replace :: T.Text -> T.Text -> T.Text -> T.Text
replace old new = T.intercalate new . T.splitOn old

numeric_string :: T.Text -> T.Text
numeric_string = T.filter (\c -> c `elem` ['1' .. '9'])

f :: T.Text -> Char
f = T.head . numeric_string

l :: T.Text -> Char
l = T.last . numeric_string

extractFirstAndLast :: T.Text -> [Char]
extractFirstAndLast = (\s -> [f s] ++ [l s])
