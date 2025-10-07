module AOC2016.Day05
  ( part1,
    part2,
  )
where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 qualified as C8
import Data.Char (digitToInt, ord)
import Data.List (sortOn)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)

part1 :: T.Text -> String
part1 input = flip C8.index 5 . fst <$> passwordHashes
  where
    prefix = C8.pack "00000"
    input' = encodeUtf8 input
    passwordHashes = foldl (\acc _ -> acc ++ [getHash (snd (last acc) + 1)]) [getHash 0] ([1 .. 7] :: [Int])
    getHash = findHash input' prefix

part2 :: T.Text -> String
part2 input = fst <$> sortOn snd (S.toList pwdList)
  where
    prefix = C8.pack "00000"
    input' = encodeUtf8 input
    pwdList = interestingHash input' S.empty prefix firstHash 0
    firstHash = fst $ findHash input' prefix 0

type Password = S.Set (Char, Int)

interestingHash :: C8.ByteString -> Password -> C8.ByteString -> C8.ByteString -> Int -> Password
interestingHash input pwd prefix currentHash n
  | S.size pwd == 8 = pwd
  | isValidChar && not indexFound =
      interestingHash input (S.insert (pwdChar, pwdIndex') pwd) prefix newHash newN
  | otherwise = interestingHash input pwd prefix newHash newN
  where
    isValidChar = ord pwdIndex >= 48 && ord pwdIndex <= 55
    indexFound = any ((== pwdIndex') . snd) pwd
    pwdIndex = C8.index currentHash 5
    pwdIndex' = digitToInt pwdIndex
    pwdChar = C8.index currentHash 6
    (newHash, newN) = findHash input prefix (n + 1)

findHash :: C8.ByteString -> C8.ByteString -> Int -> (C8.ByteString, Int)
findHash input prefix num =
  if prefix `C8.isPrefixOf` md5Hash
    then (md5Hash, num)
    else findHash input prefix $ num + 1
  where
    md5Hash = encode . MD5.hash $ input <> (C8.pack . show $ num)
