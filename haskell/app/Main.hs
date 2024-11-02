module Main where

import Util.AOCHelpers (printAoCDay)

main :: IO ()
main = do
  putStrLn "---------Advent of Code 2014---------"
  printAoC2014
  putStrLn "---------Advent of Code 2015---------"
  printAoC2015
  putStrLn "---------Advent of Code 2016---------"
  printAoC2016
  putStrLn "---------Advent of Code 2023---------"
  printAoC2023

-- putStrLn ""
-- putStrLn "Which year?"
-- year <- readLn :: IO Int

-- putStrLn "Which day?"
-- day <- readLn :: IO Int

-- printAoCDay (year, day)

printAoC2023 :: IO ()
printAoC2023 = do
  printAoCDay (2023, 01)

printAoC2015 :: IO ()
printAoC2015 = do
  printAoCDay (2015, 01)
  printAoCDay (2015, 02)
  printAoCDay (2015, 03)
  printAoCDay (2015, 04)
  printAoCDay (2015, 05)
  printAoCDay (2015, 06)
  printAoCDay (2015, 07)
  printAoCDay (2015, 08)
  printAoCDay (2015, 09)
  printAoCDay (2015, 10)
  printAoCDay (2015, 11)
  printAoCDay (2015, 12)
  printAoCDay (2015, 13)

printAoC2016 :: IO ()
printAoC2016 = do
  printAoCDay (2016, 01)

printAoC2014 :: IO ()
printAoC2014 = do
  printAoCDay (2014, 01)
