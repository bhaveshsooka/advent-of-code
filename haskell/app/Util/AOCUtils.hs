module Util.AOCUtils
  ( printDay,
    printYear,
    printYears,
  )
where

import AOC2015.Module qualified as AOC2015
import AOC2016.Module qualified as AOC2016
import AOC2018.Module qualified as AOC2018
import AOC2022.Module qualified as AOC2022
import AOC2023.Module qualified as AOC2023
import AOC2024.Module qualified as AOC2024
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock qualified as Clock
import GHC.IO (evaluate)
import Model (AOCDayPart (AOCDayPart), AOCPartsResult)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hCookie)
import Network.HTTP.Types.Status (statusCode)
import System.Directory qualified as Dir
import System.Environment (getEnv)
import Text.Printf (printf)
import Util.TimeUtils (formatNominalDiffTime)

printYears :: Int -> IO ()
printYears currentYear = mapM_ printYear [2015 .. currentYear]

printYear :: Int -> IO ()
printYear year = printTable False (year, [1 .. 25])

printDay :: (Int, Int) -> IO ()
printDay (year, day) = printTable True (year, [day])

printTable :: Bool -> (Int, [Int]) -> IO ()
printTable True (year, days) = do
  printf "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n"
  printf "┃ Advent of Code%2s                                                               %4d ┃\n" "" year
  printf "┣━━━━━┳━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━┫\n"
  printf "┃ Day ┃     Part 1 Answer ┃       Part 1 Time ┃     Part 2 Answer ┃       Part 2 Time ┃\n"
  printf "┣━━━━━╋━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━┫\n"
  mapM_ (printTableRow True year) days
  printf "┗━━━━━┻━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━┛\n"
printTable False (year, days) = do
  printf "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n"
  printf "┃ Advent of Code%2s                       %4d ┃\n" "" year
  printf "┣━━━━━┳━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━┫\n"
  printf "┃ Day ┃       Part 1 Time ┃       Part 2 Time ┃\n"
  printf "┣━━━━━╋━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━┫\n"
  mapM_ (printTableRow False year) days
  printf "┗━━━━━┻━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━┛\n"

printTableRow :: Bool -> Int -> Int -> IO ()
printTableRow includeAnswers year day = do
  let dayStr = if day < 10 then "0" ++ show day else show day
  result <- getAOCPartsResult (year, day)
  ((r1, t1), (r2, t2)) <- case result of
    Left err -> pure ((err, "N/A"), (err, "N/A"))
    Right (part1, part2) -> extractAndRunParts part1 part2
  if includeAnswers
    then printf "┃ %3s ┃ %17s ┃ %17s ┃ %17s ┃ %17s ┃\n" dayStr r1 t1 r2 t2
    else printf "┃ %3s ┃ %17s ┃ %17s ┃\n" dayStr t1 t2
  where
    extractAndRunParts part1 part2 = do
      inputData <- fetchData (year, day)
      (r1, t1) <- benchPart part1 inputData
      (r2, t2) <- benchPart part2 inputData
      pure
        ( (show r1, formatNominalDiffTime t1),
          (show r2, formatNominalDiffTime t2)
        )

benchPart :: AOCDayPart -> T.Text -> IO (String, Clock.NominalDiffTime)
benchPart (AOCDayPart part) input = do
  start <- Clock.getCurrentTime
  r <- evaluate (part input)
  end <- Clock.getCurrentTime
  pure (show r, Clock.diffUTCTime end start)

getAOCPartsResult :: (Int, Int) -> IO AOCPartsResult
getAOCPartsResult (year, day) =
  pure $ case year of
    2015 -> AOC2015.getParts day
    2016 -> AOC2016.getParts day
    2018 -> AOC2018.getParts day
    2022 -> AOC2022.getParts day
    2023 -> AOC2023.getParts day
    2024 -> AOC2024.getParts day
    _ -> Left $ "Year " <> show year <> " has not been attempted yet"

fetchData :: (Int, Int) -> IO T.Text
fetchData (year, day) = do
  let dataDir = "data/"
      yearDir = dataDir <> show year <> "/"
      paddedDay = if day < 10 then "0" <> show day else show day
      local_file = paddedDay <> ".txt"
  _ <- do
    exists <- Dir.doesDirectoryExist dataDir
    unless exists $ Dir.createDirectory dataDir
  _ <- do
    exists <- Dir.doesDirectoryExist yearDir
    unless exists $ Dir.createDirectory yearDir
  _ <- do
    exists <- Dir.doesFileExist (yearDir <> local_file)
    unless exists $ downloadFile (year, day) (yearDir <> local_file)
  byteStrData <- B.readFile (yearDir <> local_file)
  pure $ TE.decodeUtf8 byteStrData

downloadFile :: (Int, Int) -> String -> IO ()
downloadFile (year, day) filename = do
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
  cookie <- getEnv "COOKIE"
  req <- HTTP.parseRequest url
  let req0 = req {HTTP.requestHeaders = [(hCookie, C8.pack cookie)]}
  manager <- HTTP.newManager tlsManagerSettings
  resp <- HTTP.httpLbs req0 manager
  let body :: String = LC8.unpack $ HTTP.responseBody resp
  case statusCode (HTTP.responseStatus resp) of
    200 -> writeFile filename body
    _ -> error $ "Failed to download input for year " ++ show year ++ " day " ++ show day ++ ": " ++ body
