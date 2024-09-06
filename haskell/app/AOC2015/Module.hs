module AOC2015.Module (
  printAoC2015,
)
where

import AOC2015.Day01 (printAoC2015Day01Answer)
import AOC2015.Day02 (printAoC2015Day02Answer)
import AOC2015.Day03 (printAoC2015Day03Answer)
import AOC2015.Day04 (printAoC2015Day04Answer)
import AOC2015.Day05 (printAoC2015Day05Answer)

printAoC2015 :: IO ()
printAoC2015 = do
  printAoC2015Day01Answer
  printAoC2015Day02Answer
  printAoC2015Day03Answer
  printAoC2015Day04Answer
  printAoC2015Day05Answer
