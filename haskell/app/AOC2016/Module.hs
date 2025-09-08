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
    _ -> AOCNoDay
