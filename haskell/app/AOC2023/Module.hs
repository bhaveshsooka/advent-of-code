module AOC2023.Module
  ( getParts,
  )
where

import AOC2023.Day01 qualified as Day01
import AOC2023.Day02 qualified as Day02
import AOC2023.Day03 qualified as Day03
import AOC2023.Day04 qualified as Day04
import Model (AOCDayPart (AOCDayPart), AOCPartsResult)

getParts :: Int -> AOCPartsResult
getParts day =
  case day of
    01 -> Right (AOCDayPart Day01.part1, AOCDayPart Day01.part2)
    02 -> Right (AOCDayPart Day02.part1, AOCDayPart Day02.part2)
    03 -> Right (AOCDayPart Day03.part1, AOCDayPart Day03.part2)
    04 -> Right (AOCDayPart Day04.part1, AOCDayPart Day04.part2)
    _ -> Left $ "Day " <> show day <> " for 2023 has not been attempted yet"
