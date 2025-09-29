{-# LANGUAGE BangPatterns #-}

module AOC2015.Day04
  ( part1,
    part2,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, waitAnyCancel)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import System.IO.Unsafe (unsafePerformIO)

part1 :: Text -> Int
part1 input = unsafePerformIO $ parallelFind (encodeUtf8 input) isPrefix 0
  where
    isPrefix d = BS.index d 0 == 0 && BS.index d 1 == 0 && BS.index d 2 <= 0x0F

part2 :: Text -> Int
part2 input = unsafePerformIO $ parallelFind (encodeUtf8 input) isPrefix (part1 input)
  where
    isPrefix d = BS.index d 0 == 0 && BS.index d 1 == 0 && BS.index d 2 == 0

parallelFind :: ByteString -> (ByteString -> Bool) -> Int -> IO Int
parallelFind inp isPrefix start = do
  nCaps <- getNumCapabilities
  let !workers = max 1 nCaps
      launch i = async $ pure $! findNumStride (start + i) workers
  as <- mapM launch [0 .. workers - 1]
  (_, val) <- waitAnyCancel as -- cancels others once one returns
  pure val
  where
    findNumStride s step =
      if isPrefix (md5WithDigits inp s)
        then s
        else findNumStride (s + step) step
    md5WithDigits prefix n = MD5.finalize (MD5.update (MD5.update MD5.init prefix) (decBytes n))
    decBytes n0 = BS.pack (go n0 [])
      where
        go 0 [] = [48] -- "0" == 48 in ASCII
        go 0 acc = acc
        go n acc = go q (fromIntegral (48 + r) : acc)
          where
            (q, r) = n `quotRem` 10

-- import Crypto.Hash.MD5 (hash)
-- import Data.ByteString.Base16 (encode)
-- import Data.ByteString.Char8 (ByteString, isPrefixOf, pack)
-- import Data.Text qualified as T
-- import Data.Text.Encoding (encodeUtf8)

-- part1 :: T.Text -> Int
-- part1 input = findNum (encodeUtf8 input) (pack (replicate 5 '0')) 0

-- part2 :: T.Text -> Int
-- part2 input = findNum (encodeUtf8 input) (pack (replicate 6 '0')) (part1 input)

-- findNum :: ByteString -> ByteString -> Int -> Int
-- findNum input prefix num =
--   if prefix `isPrefixOf` md5Hash
--     then num
--     else findNum input prefix $ num + 1
--   where
--     md5Hash = encode . hash $ input <> (pack . show $ num)