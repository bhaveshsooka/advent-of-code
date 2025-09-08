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
import Control.Monad (forM_, unless)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.IORef (modifyIORef')
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock qualified as Clock
import GHC.IO (evaluate)
import GHC.IORef (newIORef, readIORef)
import Model
  ( AOCDayImpl (..),
    AOCDaySolution (..),
    AOCInputData,
    AOCResultStat,
    AOCResultStatRecord (..),
    AOCYearDay,
    AOCYearDays, AOCShow (aocShow),
  )
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
printYear year = printTable True (year, [1 .. 25])

printDay :: AOCYearDay -> IO ()
printDay (year, day) = printTable True (year, [day])

printTable :: Bool -> AOCYearDays -> IO ()
printTable includeAnswers (year, days) = do
  (totalTime, stats) <- runAndGetStats (year, days)
  let (tableRows, afterTableRows) =
        foldr
          ( \stat@(ResultStatRecord _ p1v _ p2v _) (rows, after) ->
              if length p1v <= 23 && length p2v <= 23
                then (stat : rows, after)
                else (rows, stat : after)
          )
          ([], [])
          stats
  printTableHeader totalTime
  forM_ tableRows printTableRow
  printTableFooter
  forM_ afterTableRows printAfterTableRow
  printf "\n"
  where
    printAfterTableRow :: AOCResultStatRecord -> IO ()
    printAfterTableRow (ResultStatRecord day p1v p1t p2v p2t) = do
      let dayStr = if day < 10 then "0" <> show day else show day
      printf "Day: %s\n" dayStr
      printf "  Part 1 Time: %s\n" $ formatNominalDiffTime p1t
      putStrLn p1v
      printf "  Part 2 Time: %s\n" $ formatNominalDiffTime p2t
      putStr p2v

    printTableHeader :: Clock.NominalDiffTime -> IO ()
    printTableHeader totalTime =
      if includeAnswers
        then do
          printf "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n"
          printf "┃ Advent of Code                                                                %4d - Total time: %10s ┃\n" year (formatNominalDiffTime totalTime)
          printf "┣━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"
          printf "┃ Day ┃           Part 1 Answer ┃             Part 1 Time ┃           Part 2 Answer ┃             Part 2 Time ┃\n"
          printf "┣━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"
        else do
          printf "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n"
          printf "┃ Advent of Code            %4d - Total time: %10s ┃\n" year (formatNominalDiffTime totalTime)
          printf "┣━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"
          printf "┃ Day ┃             Part 1 Time ┃             Part 2 Time ┃\n"
          printf "┣━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"

    printTableRow :: AOCResultStatRecord -> IO ()
    printTableRow (ResultStatRecord day p1v p1t p2v p2t) = do
      let dayStr = if day < 10 then "0" <> show day else show day
      let t1 = formatNominalDiffTime p1t
      let t2 = formatNominalDiffTime p2t
      if includeAnswers
        then printf "┃ %3s ┃ %23s ┃ %23s ┃ %23s ┃ %23s ┃\n" dayStr p1v t1 p2v t2
        else printf "┃ %3s ┃ %23s ┃ %23s ┃\n" dayStr t1 t2

    printTableFooter :: IO ()
    printTableFooter =
      if includeAnswers
        then do
          printf "┗━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┛\n"
        else do
          printf "┗━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┛\n"

runAndGetStats :: AOCYearDays -> IO AOCResultStat
runAndGetStats (year, days) = do
  totalTimeRef <- newIORef 0
  resultsRef <- newIORef []
  forM_ days $ \day -> do
    impl <- getParts (year, day)
    processImpl impl day totalTimeRef resultsRef
  totalTime <- readIORef totalTimeRef
  results <- readIORef resultsRef
  pure (totalTime, results)
  where
    mkRec = ResultStatRecord
    processImpl impl day ttRef resRef = case impl of
      AOCNoYear -> updateRefs (AOCDayNoSolution "") day ttRef resRef
      AOCNoDay -> updateRefs (AOCDayNoSolution "") day ttRef resRef
      _ -> do
        input <- fetchData (year, day)
        result <- benchImpl impl input
        updateRefs result day ttRef resRef
    updateRefs res day ttRef resRef = case res of
      AOCDayNoSolution _ ->
        modifyIORef' resRef (<> [mkRec day "" 0 "" 0])
      AOCDayPartSolution (r1, t1) -> do
        modifyIORef' ttRef (+ t1)
        modifyIORef' resRef (<> [mkRec day r1 t1 "" 0])
      AOCDayPartsSolution (r1, t1) (r2, t2) -> do
        modifyIORef' ttRef (+ (t1 + t2))
        modifyIORef' resRef (<> [mkRec day r1 t1 r2 t2])

benchImpl :: AOCDayImpl -> AOCInputData -> IO AOCDaySolution
benchImpl (AOCPartFunction part) input = do
  start <- Clock.getCurrentTime
  r <- evaluate (part input)
  end <- Clock.getCurrentTime
  pure $ AOCDayPartSolution (aocShow r, Clock.diffUTCTime end start)
benchImpl (AOCPartsFunction part1 part2) input = do
  s1 <- Clock.getCurrentTime
  r1 <- evaluate (part1 input)
  e1 <- Clock.getCurrentTime
  s2 <- Clock.getCurrentTime
  r2 <- evaluate (part2 input)
  e2 <- Clock.getCurrentTime
  pure $ AOCDayPartsSolution (aocShow r1, Clock.diffUTCTime e1 s1) (aocShow r2, Clock.diffUTCTime e2 s2)
benchImpl AOCNoDay _ = pure $ AOCDayNoSolution "This day is not yet implemented."
benchImpl AOCNoYear _ = pure $ AOCDayNoSolution "This year is not yet implemented."

getParts :: AOCYearDay -> IO AOCDayImpl
getParts (year, day) =
  pure $ case year of
    2015 -> AOC2015.getParts day
    2016 -> AOC2016.getParts day
    2018 -> AOC2018.getParts day
    2022 -> AOC2022.getParts day
    2023 -> AOC2023.getParts day
    2024 -> AOC2024.getParts day
    _ -> AOCNoYear

fetchData :: AOCYearDay -> IO AOCInputData
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
  pure $ (T.strip . TE.decodeUtf8) byteStrData

downloadFile :: AOCYearDay -> String -> IO ()
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
