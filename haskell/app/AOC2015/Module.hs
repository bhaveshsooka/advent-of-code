module AOC2015.Module (
  printAoC2015,
)
where

import AOC2015.Day01 qualified as Day01
import AOC2015.Day02 qualified as Day02
import AOC2015.Day03 qualified as Day03
import AOC2015.Day04 qualified as Day04
import AOC2015.Day05 (printAoC2015Day05Answer)
import AOC2015.Day06 (printAoC2015Day06Answer)
import AOC2015.Day07GPT (printAoC2015Day07GPTAnswer)
import AOC2015.Day08 (printAoC2015Day08Answer)
import AOC2015.Day09 (printAoC2015Day09Answer)
import AOC2015.Day10 (printAoC2015Day10Answer)
import Util.AOCHelpers (printDay)

year :: Int
year = 2015

printAoC2015 :: IO ()
printAoC2015 = do
  printDay (year, 1) Day01.parts
  printDay (year, 2) Day02.parts
  printDay (year, 3) Day03.parts
  printDay (year, 4) Day04.parts
  printAoC2015Day05Answer
  printAoC2015Day06Answer
  printAoC2015Day07GPTAnswer
  printAoC2015Day08Answer
  printAoC2015Day09Answer
  printAoC2015Day10Answer
