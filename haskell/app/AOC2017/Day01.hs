module AOC2017.Day01
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = calcCaptcha (T.head input) 0 (T.unpack input) 1 0

part2 :: T.Text -> Int
part2 input = calcCaptcha (T.head input) 0 (T.unpack input) (T.length input `div` 2) 0

calcCaptcha :: Char -> Int -> [Char] -> Int -> Int -> Int
calcCaptcha pc i input skip acc
  | i == n - 1 = if nc == pc then acc + toAdd else acc
  | nc == pc = calcCaptcha (input !! nexti) nexti input skip (acc + toAdd)
  | otherwise = calcCaptcha (input !! nexti) nexti input skip acc
  where
    toAdd = digitToInt $ input !! i
    nexti = i + 1
    nc = input !! ((i + skip) `mod` n)
    n = length input

