module AOC2016.Module
  ( getParts,
  )
where

import AOC2016.Day01 qualified as Day01
import AOC2016.Day02 qualified as Day02
import AOC2016.Day03 qualified as Day03
import AOC2016.Day04 qualified as Day04
import AOC2016.Day05 qualified as Day05
import AOC2016.Day06 qualified as Day06
import AOC2016.Day07 qualified as Day07
import AOC2016.Day08 qualified as Day08
import AOC2016.Day09 qualified as Day09
import Model (Part (Part), Parts, errMsgParts)

getParts :: Int -> Parts
getParts day =
  case day of
    01 -> (Part Day01.part1, Part Day01.part2)
    02 -> (Part Day02.part1, Part Day02.part2)
    03 -> (Part Day03.part1, Part Day03.part2)
    04 -> (Part Day04.part1, Part Day04.part2)
    05 -> (Part Day05.part1, Part Day05.part2)
    06 -> (Part Day06.part1, Part Day06.part2)
    07 -> (Part Day07.part1, Part Day07.part2)
    08 -> (Part Day08.part1, Part Day08.part2)
    09 -> (Part Day09.part1, Part Day09.part2)
    _ -> errMsgParts errMsg
  where
    errMsg = "Day " <> show day <> " for 2016 has not been attempted yet"