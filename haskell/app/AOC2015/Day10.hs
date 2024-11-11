module AOC2015.Day10
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = T.length $ repeatDescribe 40 input

part2 :: T.Text -> Int
part2 input = T.length $ repeatDescribe 50 input

repeatDescribe :: Int -> T.Text -> T.Text
repeatDescribe n s = if n == 0 then s else repeatDescribe (n - 1) $ describeNumber s
  where
    describeNumber s' = T.concat $ transform <$> T.group s'
    transform s' = T.pack (show $ T.length s') <> T.take 1 s'