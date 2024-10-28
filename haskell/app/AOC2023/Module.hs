module AOC2023.Module (
  getParts,
)
where

import AOC2023.Day01 qualified as Day01
import Model (Part (Part), Parts)

getParts :: Int -> Parts
getParts day =
  case day of
    01 -> (Part Day01.part1, Part Day01.part2)
    _ -> (Part $ const "Not there yet", Part $ const "Not there yet")
