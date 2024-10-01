{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day08 (
  printAoC2015Day08Answer,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

replaceProblems :: T.Text -> T.Text
replaceProblems = T.replace "\\\\" "." . T.replace "\"" "."

addProblems :: T.Text -> T.Text
addProblems = T.concatMap (\c -> if c == '"' || c == '\\' then "\\" <> T.pack [c] else T.pack [c])

removeQuotes :: T.Text -> T.Text
removeQuotes = T.drop 1 . T.dropEnd 1

countChars :: Int -> T.Text -> Int
countChars acc s =
  if before == "" && after == ""
    then acc
    else case after of
      "" -> acc + countCharsBefore 0 before
      _ -> countChars (acc + (countCharsBefore 0 before) + 1) (T.drop 4 after)
 where
  (before, after) = T.breakOn "\\x" s

countCharsBefore :: Int -> T.Text -> Int
countCharsBefore acc s =
  if before == "" && after == ""
    then acc
    else case after of
      "" -> acc + T.length before
      _ -> countCharsBefore (acc + (T.length before) + 1) (T.drop 2 after)
 where
  (before, after) = T.breakOn "\\" s

printAoC2015Day08Answer :: IO ()
printAoC2015Day08Answer = do
  input <- TIO.readFile "./data/Day08.txt"
  let stringLines = T.lines input
      fullLength = T.length <$> stringLines
      innerLength = (countChars 0) . replaceProblems . removeQuotes <$> stringLines
      inverseInnerLength = (+) 2 . T.length . addProblems <$> stringLines
  putStrLn "------ Day 08 ------"
  let part1 = (sum fullLength) - (sum innerLength)
  putStrLn $ "part1: " ++ show part1
  let part2 = (sum inverseInnerLength) - (sum fullLength)
  putStrLn $ "part2: " ++ show part2
  putStrLn ""
