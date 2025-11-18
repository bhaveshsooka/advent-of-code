module AOC2020.Module
  ( getParts,
  )
where

import AOC2020.Day01 qualified as Day01
import AOC2020.Day02 qualified as Day02
import Model (AOCDayImpl (AOCNoDay, AOCPartsFunction))

getParts :: Int -> AOCDayImpl
getParts day =
  case day of
    01 -> AOCPartsFunction Day01.part1 Day01.part2
    02 -> AOCPartsFunction Day02.part1 Day02.part2
    _ -> AOCNoDay