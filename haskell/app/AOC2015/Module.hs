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
import AOC2015.Day07GPT qualified as Day07
import AOC2015.Day08 qualified as Day08
import AOC2015.Day09 qualified as Day09
import AOC2015.Day10 qualified as Day10
import Util.AOCHelpers (printDay)

year :: Int
year = 2015

printAoC2015 :: IO ()
printAoC2015 = do
  printDay (year, 01) Day01.parts
  printDay (year, 02) Day02.parts
  printDay (year, 03) Day03.parts
  printDay (year, 04) Day04.parts
  printDay (year, 05) Day05.parts
  printDay (year, 06) Day06.parts
  printDay (year, 07) Day07.parts
  printDay (year, 08) Day08.parts
  printDay (year, 09) Day09.parts
  printDay (year, 10) Day10.parts
