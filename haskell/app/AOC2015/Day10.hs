{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day10 (
  printAoC2015Day10Answer,
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

describeNumber :: T.Text -> T.Text
describeNumber s = T.concat $ transform <$> T.group s

transform :: T.Text -> T.Text
transform s = (T.pack $ show $ T.length s) <> T.take 1 s

repeatDescribe :: Int -> T.Text -> T.Text
repeatDescribe 0 s = s
repeatDescribe n s = repeatDescribe (n - 1) $ describeNumber s

printAoC2015Day10Answer :: IO ()
printAoC2015Day10Answer = do
  input <- TIO.readFile "./data/2015-10.txt"
  putStrLn "------ Day 10 ------"
  let part1 = T.length $ repeatDescribe 40 input
  putStrLn $ "part1: " ++ show part1
  let part2 = T.length $ repeatDescribe 50 input
  putStrLn $ "part2: " ++ show part2
  putStrLn ""
