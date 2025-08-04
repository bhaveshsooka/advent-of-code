module AOC2015.Day10
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.List (group)

part1 :: T.Text -> Int
part1 input = length $ repeatDescribe 40 $ T.unpack input

part2 :: T.Text -> Int
part2 input = length $ repeatDescribe 50 $ T.unpack input

repeatDescribe :: Int -> String -> String
repeatDescribe n s = if n == 0 then s else repeatDescribe (n - 1) $ describeNumber s
  where
    describeNumber s' = concatMap transform (group s')
    transform s' = show (length s') <> take 1 s'