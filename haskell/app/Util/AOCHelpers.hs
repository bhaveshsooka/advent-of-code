module Util.AOCHelpers (
  printDay,
) where

import AOC2015.Module qualified as AOC2015
import AOC2023.Module qualified as AOC2023
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
getPartsFromAoCDay aocDay@(year, day)
  | not . validated $ aocDay = pure (partDoesNotExist, partDoesNotExist)
  | otherwise =
      pure $ case year of
        2015 -> AOC2015.getParts day
        2023 -> AOC2023.getParts day
        _ -> (partNotThereYet, partNotThereYet)
 where
  validated (y, d) = y >= 2015 && d >= 1 && d <= 25
  partDoesNotExist = Part $ const "Doesn't exist"
  partNotThereYet = Part $ const "Not there yet"

formatAoCOutput :: ((String, Double), (String, Double)) -> T.Text -> IO ()
formatAoCOutput ((part1, t1), (part2, t2)) day = do
  printf $ "------ Day " <> T.unpack day <> " ------"
  printf "\n"
  printf "part1: %s (%0.9f sec)" part1 t1
  printf "\n"
  printf "part2: %s (%0.9f sec)" part2 t2
  printf "\n\n"
