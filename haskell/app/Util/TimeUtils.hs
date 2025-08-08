{-# LANGUAGE CPP #-}

module Util.TimeUtils
  ( formatNominalDiffTime,
  )
where

import Data.Fixed (E12, Fixed)
import Data.Time.Clock
  ( NominalDiffTime,
    nominalDiffTimeToSeconds,
    secondsToNominalDiffTime,
  )
import Text.Printf (printf)

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime diff
  | diff < ps = roundFixed (nominalDiffTimeToSeconds diff * 10 ^ (15 :: Integer)) <> "fs"
  | diff < ns = roundFixed (nominalDiffTimeToSeconds diff * 10 ^ (12 :: Integer)) <> "ps"
  | diff < us = roundFixed (nominalDiffTimeToSeconds diff * 10 ^ (9 :: Integer)) <> "ns"
  | diff < ms = roundFixed (nominalDiffTimeToSeconds diff * 10 ^ (6 :: Integer)) <> "us"
  | diff < s = roundFixed (nominalDiffTimeToSeconds diff * 10 ^ (3 :: Integer)) <> "ms"
  | diff < m = roundFixed (nominalDiffTimeToSeconds diff) <> "s"
  | otherwise = roundFixed (nominalDiffTimeToSeconds diff / 60) <> "m"

roundFixed :: Fixed E12 -> String
roundFixed f = printf "%.2f" (realToFrac f :: Double)

m, s, ms, us, ns, ps :: NominalDiffTime
m = secondsToNominalDiffTime 60
s = secondsToNominalDiffTime 1
ms = secondsToNominalDiffTime 0.001
us = secondsToNominalDiffTime 0.000001
ns = secondsToNominalDiffTime 0.000000001
ps = secondsToNominalDiffTime 0.000000000001

#if !MIN_VERSION_time(1,9,1)
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac
#endif