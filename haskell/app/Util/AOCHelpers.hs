module Util.AOCHelpers
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
import Data.ByteString.Char8 qualified as Char8 (pack)
import Data.ByteString.Lazy.Char8 qualified as LChar8 (unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import GHC.IO (evaluate)
import Model (AOCDayPart (AOCDayPart), AOCPartsResult)
import Network.HTTP.Client
  ( httpLbs,
    newManager,
    parseRequest,
    requestHeaders,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hCookie)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv)
import Text.Printf (printf)
import Util.TimeUtils (formatNominalDiffTime)

printYears :: Int -> IO ()
printYears currentYear = mapM_ printYear [2015 .. currentYear]

printYear :: Int -> IO ()
printYear y = do
  putStrLn $ "---------Advent of Code " <> show y <> "---------"
  mapM_ printDay ([(y, day) | day <- [1 .. 25]])
  putStrLn ""

printDay :: (Int, Int) -> IO ()
printDay (year, day) = do
  result <- getAoCResult (year, day)
  case result of
    Left err -> do
      printf err
      printf "\n"
    Right (part1, part2) -> do
      inputData <- fetchData (year, day)
      (p1, t1) <- benchPart part1 inputData
      (p2, t2) <- benchPart part2 inputData
      let resultText =
            "Year "
              ++ show year
              ++ ", Day "
              ++ (if day < 10 then "0" ++ show day else show day)
              ++ " -> Part 1: "
              ++ p1
              ++ ", Time: "
              ++ formatNominalDiffTime t1
              ++ ", Part 2: "
              ++ p2
              ++ ", Time: "
              ++ formatNominalDiffTime t2
      printf resultText
      printf "\n"

benchPart :: AOCDayPart -> T.Text -> IO (String, NominalDiffTime)
benchPart (AOCDayPart part) input = do
  start <- getCurrentTime
  r <- evaluate (part input) -- only forces to WHNF
  end <- getCurrentTime
  pure (show r, diffUTCTime end start)

getAoCResult :: (Int, Int) -> IO AOCPartsResult
getAoCResult (year, day) =
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
    exists <- doesDirectoryExist dataDir
    unless exists $ createDirectory dataDir
  _ <- do
    exists <- doesDirectoryExist yearDir
    unless exists $ createDirectory yearDir
  _ <- do
    exists <- doesFileExist (yearDir <> local_file)
    unless exists $ downloadFile (year, day) (yearDir <> local_file)
  byteStrData <- B.readFile (yearDir <> local_file)
  pure $ TE.decodeUtf8 byteStrData

downloadFile :: (Int, Int) -> String -> IO ()
downloadFile (year, day) filename = do
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
  cookie <- getEnv "COOKIE"
  req <- parseRequest url
  let req0 = req {requestHeaders = [(hCookie, Char8.pack cookie)]}
  manager <- newManager tlsManagerSettings
  resp <- httpLbs req0 manager
  let body :: String = LChar8.unpack $ responseBody resp
  case statusCode (responseStatus resp) of
    200 -> do
      writeFile filename body
    _ -> do
      error $ "Failed to download input for year " ++ show year ++ " day " ++ show day ++ ": " ++ body
