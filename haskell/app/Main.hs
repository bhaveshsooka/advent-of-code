module Main where

import AOC2015.Module (printAoC2015)
import AOC2023.Module (printAoC2023)
-- import Util.AOCHelpers (printDay)

main :: IO ()
main = do
  putStrLn "---------Advent of Code 2015---------"
  printAoC2015
  putStrLn "---------Advent of Code 2023---------"
  printAoC2023

-- putStrLn ""
-- putStrLn "Which year?"
-- year <- readLn :: IO Int

-- putStrLn "Which day?"
-- day <- readLn :: IO Int

-- printDay (year, day)
