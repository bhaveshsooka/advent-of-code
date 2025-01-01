module AOC2016.Day05
  ( part1,
    part2,
  )
where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as C8
import Data.Char (digitToInt, isDigit)
import Data.List ((!?))
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

part1 :: T.Text -> String
part1 input = (\(h, _) -> C8.head $ C8.drop 5 h) <$> passwordHashes
  where
    passwordHashes = foldl (\acc _ -> acc ++ [getHash (snd (last acc) + 1)]) [getHash 0] passwordLength
    passwordLength = [1 .. 7] :: [Int]
    getHash = findHash (encodeUtf8 input) (C8.pack (replicate 5 '0'))

part2 :: T.Text -> String
part2 input = decryptingHash input ('_', '_', '_', '_', '_', '_', '_', '_') (getHash 0)
  where
    getHash = findHash (encodeUtf8 input) (C8.pack (replicate 5 '0'))

decryptingHash ::
  T.Text ->
  (Char, Char, Char, Char, Char, Char, Char, Char) ->
  (C8.ByteString, Int) ->
  String
decryptingHash input (a, b, c, d, e, f, g, h) (currentHash, currentNum) =
  if '_' `elem` currentPassword
    then
      if isDigit (C8.head $ C8.drop 5 currentHash)
        then case currentPassword !? digitToInt (C8.head $ C8.drop 5 currentHash) of
          Just '_' -> decryptingHash input newPassword (newHash, newNum + 1)
          _ -> decryptingHash input (a, b, c, d, e, f, g, h) (newHash, newNum + 1)
        else decryptingHash input (a, b, c, d, e, f, g, h) (newHash, newNum + 1)
    else currentPassword
  where
    currentPassword = [a, b, c, d, e, f, g, h]
    newPassword = case C8.head $ C8.drop 5 currentHash of
      '0' -> (value, b, c, d, e, f, g, h)
      '1' -> (a, value, c, d, e, f, g, h)
      '2' -> (a, b, value, d, e, f, g, h)
      '3' -> (a, b, c, value, e, f, g, h)
      '4' -> (a, b, c, d, value, f, g, h)
      '5' -> (a, b, c, d, e, value, g, h)
      '6' -> (a, b, c, d, e, f, value, h)
      '7' -> (a, b, c, d, e, f, g, value)
      _ -> (a, b, c, d, e, f, g, h)
    value = C8.head $ C8.drop 6 currentHash
    (newHash, newNum) = findHash (encodeUtf8 input) (C8.pack (replicate 5 '0')) (currentNum + 1)

findHash :: C8.ByteString -> C8.ByteString -> Int -> (C8.ByteString, Int)
findHash input prefix num =
  if prefix `C8.isPrefixOf` md5Hash
    then (md5Hash, num)
    else findHash input prefix $ num + 1
  where
    md5Hash = encode . hash $ input <> (C8.pack . show $ num)
