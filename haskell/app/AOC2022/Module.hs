module AOC2022.Module
  ( getParts,
  )
where

import AOC2022.Day01 qualified as Day01
import AOC2022.Day02 qualified as Day02
import Model (AOCDayPart (AOCDayPart), AOCPartsResult)

getParts :: Int -> AOCPartsResult
getParts day =
  case day of
    01 -> Right (AOCDayPart Day01.part1, AOCDayPart Day01.part2)
    02 -> Right (AOCDayPart Day02.part1, AOCDayPart Day02.part2)
    _ -> Left $ "Day " <> show day <> " for 2022 has not been attempted yet"
