module AOC2023.Module
  ( getParts,
  )
where

import AOC2023.Day01 qualified as Day01
import AOC2023.Day02 qualified as Day02
import AOC2023.Day03 qualified as Day03
import AOC2023.Day04 qualified as Day04
import Model (Part (Part), Parts, errMsgParts)

getParts :: Int -> Parts
getParts day =
  case day of
    01 -> (Part Day01.part1, Part Day01.part2)
    02 -> (Part Day02.part1, Part Day02.part2)
    03 -> (Part Day03.part1, Part Day03.part2)
    04 -> (Part Day04.part1, Part Day04.part2)
    _ -> errMsgParts errMsg
  where
    errMsg = "Day " <> show day <> " for 2023 has not been attempted yet"