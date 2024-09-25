module AOC2015.Day04 (
  printAoC2015Day04Answer,
) where

import Prelude hiding (take)
import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.UTF8 (fromString, take)

printAoC2015Day04Answer :: IO ()
printAoC2015Day04Answer = do
  putStrLn "------ Day 04 ------"
  putStrLn $ "part1: " ++ show part1
  putStrLn $ "part2: " ++ show part2
  putStrLn ""

input :: ByteString
input = fromString  "ckczppom"

part1 :: Int
part1 = findNum input 0
 where
  findNum :: ByteString -> Int -> Int
  findNum h num 
    | take 5 h == fromString "00000" = num
    | otherwise = findNum (md5Hash $ input <> (pack . show $ num + 1)) (num + 1)

part2 :: Int
part2 = findNum input $ part1
 where
  findNum :: ByteString -> Int -> Int
  findNum h num 
    | (take 6 h) == fromString "000000" = num
    | otherwise = findNum (md5Hash $ input <> (pack . show $ num + 1)) (num + 1)

md5Hash :: ByteString -> ByteString
md5Hash = encode . hash
