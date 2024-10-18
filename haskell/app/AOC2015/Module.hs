module AOC2015.Module (
  printAoC2015,
)
where

import AOC2015.Day01 qualified as Day01
import AOC2015.Day02 qualified as Day02
import AOC2015.Day03 qualified as Day03
import AOC2015.Day04 qualified as Day04
import AOC2015.Day05 qualified as Day05
import AOC2015.Day06 qualified as Day06
import AOC2015.Day07GPT (printAoC2015Day07GPTAnswer)
import AOC2015.Day08 qualified as Day08
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
  printDay (year, 5) Day05.parts
  printDay (year, 6) Day06.parts
  printAoC2015Day07GPTAnswer
  printDay (year, 8) Day08.parts
  printAoC2015Day09Answer
  printAoC2015Day10Answer
