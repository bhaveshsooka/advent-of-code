module AOC2018.Module
  ( getParts,
  )
where

import AOC2018.Day01 qualified as Day01
import Model (AOCDayPart (AOCDayPart), AOCPartsResult)

getParts :: Int -> AOCPartsResult
getParts day =
  case day of
    01 -> Right (AOCDayPart Day01.part1, AOCDayPart Day01.part2)
    _ -> Left $ "Day " <> show day <> " for 2018 has not been attempted yet"