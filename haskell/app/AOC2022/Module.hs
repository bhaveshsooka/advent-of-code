module AOC2022.Module
  ( getParts,
  )
where

import AOC2022.Day01 qualified as Day01
import AOC2022.Day02 qualified as Day02
import Model (Part (Part), Parts, errMsgParts)

getParts :: Int -> Parts
getParts day =
  case day of
    01 -> (Part Day01.part1, Part Day01.part2)
    02 -> (Part Day02.part1, Part Day02.part2)
    _ -> errMsgParts errMsg
  where
    errMsg = "Day " <> show day <> " for 2022 has not been attempted yet"