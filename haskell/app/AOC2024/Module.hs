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
import Model (AOCDayImpl (AOCNoDay, AOCPartsFunction))

getParts :: Int -> AOCDayImpl
getParts day =
  case day of
    01 -> AOCPartsFunction Day01.part1 Day01.part2
    02 -> AOCPartsFunction Day02.part1 Day02.part2
    03 -> AOCPartsFunction Day03.part1 Day03.part2
    04 -> AOCPartsFunction Day04.part1 Day04.part2
    05 -> AOCPartsFunction Day05.part1 Day05.part2
    06 -> AOCPartsFunction Day06.part1 Day06.part2
    07 -> AOCPartsFunction Day07.part1 Day07.part2
    08 -> AOCPartsFunction Day08.part1 Day08.part2
    09 -> AOCPartsFunction Day09.part1 Day09.part2
    10 -> AOCPartsFunction Day10.part1 Day10.part2
    11 -> AOCPartsFunction Day11.part1 Day11.part2
    13 -> AOCPartsFunction Day13.part1 Day13.part2
    14 -> AOCPartsFunction Day14.part1 Day14.part2
    17 -> AOCPartsFunction Day17.part1 Day17.part2
    22 -> AOCPartsFunction Day22.part1 Day22.part2
    23 -> AOCPartsFunction Day23.part1 Day23.part2
    24 -> AOCPartsFunction Day24.part1 Day24.part2
    25 -> AOCPartsFunction Day25.part1 Day25.part2
    _ -> AOCNoDay