module AOC2015.Day04 (
  parts,
) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, pack, take)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Util.AOCHelpers (Part (Part), Parts)
import Prelude hiding (take)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = findNum (encodeUtf8 input) (encodeUtf8 input) 5 0

part2 :: T.Text -> Int
part2 input = findNum (encodeUtf8 input) (encodeUtf8 input) 6 (part1 input)

findNum :: ByteString -> ByteString -> Int -> Int -> Int
findNum input h count num =
  if take count h == pack (replicate count '0')
    then num
    else findNum input (encode . hash $ newInput) count (num + 1)
 where
  newInput = input <> (pack . show $ num + 1)
