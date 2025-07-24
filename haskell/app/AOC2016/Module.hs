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
import Model (AOCDayPart (AOCDayPart), AOCPartsResult)

getParts :: Int -> AOCPartsResult
getParts day =
  case day of
    01 -> Right (AOCDayPart Day01.part1, AOCDayPart Day01.part2)
    02 -> Right (AOCDayPart Day02.part1, AOCDayPart Day02.part2)
    03 -> Right (AOCDayPart Day03.part1, AOCDayPart Day03.part2)
    04 -> Right (AOCDayPart Day04.part1, AOCDayPart Day04.part2)
    05 -> Right (AOCDayPart Day05.part1, AOCDayPart Day05.part2)
    06 -> Right (AOCDayPart Day06.part1, AOCDayPart Day06.part2)
    07 -> Right (AOCDayPart Day07.part1, AOCDayPart Day07.part2)
    08 -> Right (AOCDayPart Day08.part1, AOCDayPart Day08.part2)
    09 -> Right (AOCDayPart Day09.part1, AOCDayPart Day09.part2)
    _ -> Left $ "Day " <> show day <> " for 2016 has not been attempted yet"