{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day08 (
  part1,
  part2,
) where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum (T.length <$> T.lines input) - sum innerLength
 where
  innerLength = countChars 0 . replaceProblems . T.drop 1 . T.dropEnd 1 <$> T.lines input

part2 :: T.Text -> Int
part2 input = sum inverseInnerLength - sum (T.length <$> T.lines input)
 where
  inverseInnerLength = (+) 2 . T.length . addProblems <$> T.lines input

replaceProblems :: T.Text -> T.Text
replaceProblems = T.replace "\\\\" "." . T.replace "\"" "."

addProblems :: T.Text -> T.Text
addProblems = T.concatMap (\c -> if c == '"' || c == '\\' then "\\" <> T.pack [c] else T.pack [c])

countChars :: Int -> T.Text -> Int
countChars acc s =
  if before == "" && after == ""
    then acc
    else case after of
      "" -> acc + countCharsBefore 0 before
      _ -> countChars (acc + countCharsBefore 0 before + 1) (T.drop 4 after)
 where
  (before, after) = T.breakOn "\\x" s

countCharsBefore :: Int -> T.Text -> Int
countCharsBefore acc s =
  if before == "" && after == ""
    then acc
    else case after of
      "" -> acc + T.length before
      _ -> countCharsBefore (acc + T.length before + 1) (T.drop 2 after)
 where
  (before, after) = T.breakOn "\\" s
