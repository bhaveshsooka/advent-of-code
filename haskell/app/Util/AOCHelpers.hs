{-# LANGUAGE OverloadedStrings #-}

module Util.AOCHelpers (
  printAoCDay,
) where

import AOC2015.Module qualified as AOC2015
import AOC2023.Module qualified as AOC2023
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Formatting (fixed, format, text, (%))
import Model (AoCAnswer, AoCDay, Part (Part), Parts, Timing (NoValue, Value), errMsgParts)
import System.IO.Error (tryIOError)
import Text.Printf (printf)

printAoCDay :: AoCDay -> IO ()
printAoCDay (year, day) =
  if not validated
    then do
      printf $ "------ Day " <> T.unpack d <> " ------"
      printf $ "\npart1: " <> errMsgInvalidYear
      printf $ "\npart2: " <> errMsgInvalidYear
      printf "\n\n"
    else do
      (Part part1, Part part2) <- getAoCDayParts (year, day)
      inputTextResult <- tryIOError $ TIO.readFile filename
      (p1, p2) <- pure $ case inputTextResult of
        Left _ -> (errMsg, errMsg)
        Right a -> (runPart part1 a, runPart part2 a)
      printf $ "------ Day " <> T.unpack d <> " ------"
      printf $ "\npart1: " <> p1
      printf $ "\npart2: " <> p2
      printf "\n\n"
 where
  d = padLeft (T.pack $ show day) '0' 2
  filename = "./data/" <> show year <> "-" <> T.unpack d <> ".txt"
  padLeft s c n = T.replicate (n - T.length s) (T.singleton c) <> s
  errMsg = "Input data file not found: " <> filename
  runPart p t = formatAoCAnswer (show $ p t, NoValue)
  validated = year >= 2015 && day >= 1 && day <= 25
  errMsgInvalidYear = "Invalid AoC Day " <> show year <> "-" <> T.unpack d

getAoCDayParts :: AoCDay -> IO Parts
getAoCDayParts (year, day) =
  pure $ case year of
    2015 -> AOC2015.getParts day
    2023 -> AOC2023.getParts day
    _ -> errMsgParts errMsgValidYear
 where
  errMsgValidYear = "Year " <> show year <> " has not been attempted yet"

formatAoCAnswer :: AoCAnswer -> String
formatAoCAnswer (p, t) = TL.unpack $
  case t of
    Value a -> format (text % "(" % fixed 2 % " sec)") (TL.pack p) a
    NoValue -> format text (TL.pack p)
