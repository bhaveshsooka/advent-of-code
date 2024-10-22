{-# LANGUAGE OverloadedStrings #-}

module AOC2023.Day01 (
  part1,
  part2,
) where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum $ read <$> list
 where
  list = extractFirstAndLast . numericString <$> T.lines input

part2 :: T.Text -> Int
part2 input = sum $ read <$> list
 where
  list = (extractFirstAndLast <$> numericString) . replaceWordsWithDigits <$> T.lines input

replaceWordsWithDigits :: T.Text -> T.Text
replaceWordsWithDigits str =
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

numericString :: T.Text -> T.Text
numericString = T.filter (\c -> c `elem` ['1' .. '9'])

f :: T.Text -> Char
f = T.head . numericString

l :: T.Text -> Char
l = T.last . numericString

extractFirstAndLast :: T.Text -> [Char]
extractFirstAndLast s = f s : [l s]
