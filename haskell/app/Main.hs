module Main where

import Util.AOCHelpers (printDay)

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

printAoC2023 :: IO ()
printAoC2023 = do
  printDay (2023, 01)

printAoC2015 :: IO ()
printAoC2015 = do
  printDay (2015, 01)
  printDay (2015, 02)
  printDay (2015, 03)
  printDay (2015, 04)
  printDay (2015, 05)
  printDay (2015, 06)
  printDay (2015, 07)
  printDay (2015, 08)
  printDay (2015, 09)
  printDay (2015, 10)
  printDay (2015, 11)
  printDay (2015, 12)
