module AOC2024.Module
  ( getParts,
  )
where

import AOC2024.Day01 qualified as Day01
import AOC2024.Day02 qualified as Day02
import AOC2024.Day03 qualified as Day03
import AOC2024.Day04 qualified as Day04
import AOC2024.Day05 qualified as Day05
import AOC2024.Day06 qualified as Day06
import AOC2024.Day07 qualified as Day07
import AOC2024.Day08 qualified as Day08
import AOC2024.Day09 qualified as Day09
import AOC2024.Day10 qualified as Day10
import AOC2024.Day11 qualified as Day11
import AOC2024.Day13 qualified as Day13
import AOC2024.Day14 qualified as Day14
import AOC2024.Day17 qualified as Day17
import AOC2024.Day22 qualified as Day22
import AOC2024.Day23 qualified as Day23
import AOC2024.Day24 qualified as Day24
import AOC2024.Day25 qualified as Day25
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
    10 -> Right (AOCDayPart Day10.part1, AOCDayPart Day10.part2)
    11 -> Right (AOCDayPart Day11.part1, AOCDayPart Day11.part2)
    13 -> Right (AOCDayPart Day13.part1, AOCDayPart Day13.part2)
    14 -> Right (AOCDayPart Day14.part1, AOCDayPart Day14.part2)
    17 -> Right (AOCDayPart Day17.part1, AOCDayPart Day17.part2)
    22 -> Right (AOCDayPart Day22.part1, AOCDayPart Day22.part2)
    23 -> Right (AOCDayPart Day23.part1, AOCDayPart Day23.part2)
    24 -> Right (AOCDayPart Day24.part1, AOCDayPart Day24.part2)
    25 -> Right (AOCDayPart Day25.part1, AOCDayPart Day25.part2)
    _ -> Left $ "Day " <> show day <> " for 2024 has not been attempted yet"