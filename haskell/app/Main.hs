module Main where

import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Text.Read (readMaybe)
import Util.AOCHelpers (printAoCDay, printYear, printYears)
import Configuration.Dotenv (loadFile, defaultConfig)

main :: IO ()
main = do
  loadFile defaultConfig
  yearMax <- (\(x, _, _) -> fromInteger x) . toGregorian . utctDay <$> getCurrentTime
  let yearPrompt = "Which year would you like to see?"
      yearErr = "Invalid year. Please enter a valid year between 2015 and " ++ show yearMax ++ "."
  putStrLn "Welcome to Advent of Code!"
  maybeYear <- whatToPrint yearPrompt yearErr 2015 yearMax
  maybe (printYears yearMax) handleYear maybeYear

handleYear :: Int -> IO ()
handleYear year = do
  let dayPrompt = "Which day would you like to see?"
      dayErr = "Invalid day. Please enter a valid day between 1 and 25."
  maybeDay <- whatToPrint dayPrompt dayErr 1 25
  maybe (printYear year) (handleDay year) maybeDay

handleDay :: Int -> Int -> IO ()
handleDay year day = do
  let aoCDay = (year, day)
  printAoCDay aoCDay

whatToPrint :: (Read a, Show a, Ord a) => String -> String -> a -> a -> IO (Maybe a)
whatToPrint prompt errMsg minVal maxVal = do
  putStrLn $ prompt ++ " (" ++ show minVal ++ "-" ++ show maxVal ++ " or all):"
  input <- getLine
  case (input, readMaybe input) of
    ("all", _) -> pure Nothing
    (_, Just x) | x >= minVal && x <= maxVal -> pure (Just x)
    _ -> putStrLn errMsg >> whatToPrint prompt errMsg minVal maxVal
