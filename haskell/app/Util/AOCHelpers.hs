module Util.AOCHelpers (
  printDay,
) where

import AOC2015.Day01 qualified
import AOC2015.Day02 qualified
import AOC2015.Day03 qualified
import AOC2015.Day04 qualified
import AOC2015.Day05 qualified
import AOC2015.Day06 qualified
import AOC2015.Day07 qualified
import AOC2015.Day08 qualified
import AOC2015.Day09 qualified
import AOC2015.Day10 qualified
import AOC2023.Day01 qualified
import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Model (AOC_Day, Part (Part), Parts)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

printDay :: AOC_Day -> IO ()
printDay (year, d) = do
  fileContentsOrUnit <- tryJust (guard . isDoesNotExistError) (TIO.readFile filename)
  case fileContentsOrUnit of
    Left _ -> formatAoCOutput noData day
    Right input -> do
      (Part p1, Part p2) <- getPartsFromAoCDay (year, d)
      let one = (show $ p1 input, 0)
      let two = (show $ p2 input, 0)
      formatAoCOutput (one, two) day
 where
  day = padLeft (T.pack $ show d) '0' 2
  filename = "./data/" <> show year <> "-" <> T.unpack day <> ".txt"
  noDataMessage = "File not found: " <> filename
  noData = ((noDataMessage, 0), (noDataMessage, 0))
  padLeft s c len = T.replicate (len - T.length s) (T.singleton c) <> s

getPartsFromAoCDay :: AOC_Day -> IO Parts
getPartsFromAoCDay aocDay
  | not $ validated aocDay = pure (Part $ const "Doesn't exist", Part $ const "Doesn't exist")
  | otherwise =
      pure $ case aocDay of
        (2015, 01) -> (Part AOC2015.Day01.part1, Part AOC2015.Day01.part2)
        (2015, 02) -> (Part AOC2015.Day02.part1, Part AOC2015.Day02.part2)
        (2015, 03) -> (Part AOC2015.Day03.part1, Part AOC2015.Day03.part2)
        (2015, 04) -> (Part AOC2015.Day04.part1, Part AOC2015.Day04.part2)
        (2015, 05) -> (Part AOC2015.Day05.part1, Part AOC2015.Day05.part2)
        (2015, 06) -> (Part AOC2015.Day06.part1, Part AOC2015.Day06.part2)
        (2015, 07) -> (Part AOC2015.Day07.part1, Part AOC2015.Day07.part2)
        (2015, 08) -> (Part AOC2015.Day08.part1, Part AOC2015.Day08.part2)
        (2015, 09) -> (Part AOC2015.Day09.part1, Part AOC2015.Day09.part2)
        (2015, 10) -> (Part AOC2015.Day10.part1, Part AOC2015.Day10.part2)
        (2023, 01) -> (Part AOC2023.Day01.part1, Part AOC2023.Day01.part2)
        _ -> (Part $ const "Not there yet", Part $ const "Not there yet")
 where
  validated (y, d) = y >= 2015 && d >= 1 && d <= 25

formatAoCOutput :: ((String, Double), (String, Double)) -> T.Text -> IO ()
formatAoCOutput ((part1, t1), (part2, t2)) day = do
  printf $ "------ Day " <> T.unpack day <> " ------"
  printf "\n"
  printf "part1: %s (%0.9f sec)" part1 t1
  printf "\n"
  printf "part2: %s (%0.9f sec)" part2 t2
  printf "\n\n"
