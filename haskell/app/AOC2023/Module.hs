module AOC2023.Module (
  printAoC2023,
)
where

import AOC2023.Day01 qualified as Day01
import Util.AOCHelpers (printDay)

year :: Int
year = 2023

printAoC2023 :: IO ()
printAoC2023 = do
  printDay (year, 01) Day01.parts
