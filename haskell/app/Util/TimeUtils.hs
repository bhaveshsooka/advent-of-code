module Util.TimeUtils
  ( formatNominalDiffTime,
  )
where

import Data.Fixed (E12, Fixed)
import Data.Time.Clock (NominalDiffTime)
import Text.Printf (printf)

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime diff
  | diff == 0 = "-"
  | otherwise = withColor colorCode rendered
  where
    rendered
      | diff < ps = roundFixed (realToFrac $ diff / fs) <> "fs"
      | diff < ns = roundFixed (realToFrac $ diff / ps) <> "ps"
      | diff < us = roundFixed (realToFrac $ diff / ns) <> "ns"
      | diff < ms = roundFixed (realToFrac $ diff / us) <> "us"
      | diff < s = roundFixed (realToFrac $ diff / ms) <> "ms"
      | diff < m = roundFixed (realToFrac $ diff / s) <> "s"
      | otherwise = roundFixed (realToFrac $ diff / m) <> "m"

    colorCode
      | diff < (20 * ms) = cyan
      | diff < (200 * ms) = yellow
      | otherwise = red

    withColor code text = ansi code <> text <> ansi reset
    ansi code = "\ESC[" <> code <> "m"

    roundFixed :: Fixed E12 -> String
    roundFixed f = printf "%.2f" (realToFrac f :: Double)

    red = "31"
    yellow = "33"
    cyan = "36"
    reset = "0"

    fs = 1e-15
    ps = 1e-12
    ns = 1e-9
    us = 1e-6
    ms = 1e-3
    s = 1e0
    m = 60e0
