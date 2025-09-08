module AOC2023.Module
  ( getParts,
  )
where

import AOC2023.Day01 qualified as Day01
import AOC2023.Day02 qualified as Day02
import AOC2023.Day03 qualified as Day03
import AOC2023.Day04 qualified as Day04
import Model (AOCDayImpl (AOCNoDay, AOCPartsFunction))

getParts :: Int -> AOCDayImpl
getParts day =
  case day of
    01 -> AOCPartsFunction Day01.part1 Day01.part2
    02 -> AOCPartsFunction Day02.part1 Day02.part2
    03 -> AOCPartsFunction Day03.part1 Day03.part2
    04 -> AOCPartsFunction Day04.part1 Day04.part2
    _ -> AOCNoDay
