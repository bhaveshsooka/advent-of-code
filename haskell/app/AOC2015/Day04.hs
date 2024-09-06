module AOC2015.Day04 (
  printAoC2015Day04Answer,
) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.UTF8 (fromString)

printAoC2015Day04Answer :: IO ()
printAoC2015Day04Answer = do
  putStrLn "------ Day 04 ------"
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2
  putStrLn ""

input :: String
input = "ckczppom"

part1 :: Int
part1 =
  let findNum :: String -> Int -> Int
      findNum h num =
        if (take 5 h) == "00000"
          then num
          else findNum (md5Hash $ input ++ (show $ num + 1)) (num + 1)
   in findNum (md5Hash input) 0

part2 :: Int
part2 =
  let findNum :: String -> Int -> Int
      findNum h num =
        if (take 6 h) == "000000"
          then num
          else findNum (md5Hash $ input ++ (show $ num + 1)) (num + 1)
   in findNum (md5Hash input) 0

md5Hash :: String -> String
md5Hash = unpack . encode . hash . fromString
