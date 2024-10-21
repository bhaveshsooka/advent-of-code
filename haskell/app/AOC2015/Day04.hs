module AOC2015.Day04 (
  parts,
) where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack, take)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Util.AOCHelpers (Part (Part), Parts)
import Prelude hiding (take)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = findNum (encodeUtf8 input) (pack (replicate 5 '0')) 0

part2 :: T.Text -> Int
part2 input = findNum (encodeUtf8 input) (pack (replicate 6 '0')) (part1 input)

findNum :: ByteString -> ByteString -> Int -> Int
findNum input prefix num =
  if isPrefixOf prefix md5Hash
    then num
    else findNum input prefix $ num + 1
 where
  md5Hash = encode . hash $ input <> (pack . show $ num)
