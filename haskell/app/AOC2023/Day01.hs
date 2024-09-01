module AOC2023.Day01 (
  part1,
  part2
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)

part1 :: String -> String
part1 s =
  let numeric_string = filter (\c -> c `elem` ['1'..'9']) s
      f = head numeric_string
      l = last numeric_string
  in
    [f] ++ [l]

part2 :: String -> String
part2 s =
  let replaced_string = replace_words_with_digits s
      numeric_string = filter (\c -> c `elem` ['1'..'9']) replaced_string
      f = head numeric_string
      l = last numeric_string
  in
    [f] ++ [l]

replace_words_with_digits :: String -> String
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
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old
