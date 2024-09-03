module Main where

import AOC2015.Module (printAoC2015)
import AOC2023.Module (printAoC2023)

main :: IO ()
main = do
  putStrLn "---------Advent of Code 2015---------"
  printAoC2015
  putStrLn "---------Advent of Code 2023---------"
  printAoC2023
