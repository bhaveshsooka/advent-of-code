module AOC2018.Module
  ( getParts,
  )
where

import AOC2018.Day01 qualified as Day01
import Model (Part (Part), Parts, errMsgParts)

getParts :: Int -> Parts
getParts day =
  case day of
    01 -> (Part Day01.part1, Part Day01.part2)
    _ -> errMsgParts errMsg
  where
    errMsg = "Day " <> show day <> " for 2018 has not been attempted yet"