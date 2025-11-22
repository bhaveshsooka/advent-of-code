module AOC2018.Day05
  ( part1,
    part2,
  )
where

import Data.Char (toLower, toUpper)
import Data.Function (on)
import Data.List (minimumBy, nub)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ eliminate $ T.unpack input

part2 :: T.Text -> Int
part2 input = length . minimumBy (compare `on` length) $ eliminate . replacements <$> units
  where
    replacements u = T.unpack $ T.replace (T.pack [toUpper u]) T.empty $ T.replace (T.pack [toLower u]) T.empty input
    units = nub $ T.unpack . T.toLower $ input

eliminate :: String -> String
eliminate = foldr step []
  where
    step x (y : ys)
      | x /= y && toLower x == toLower y = ys
    step x ys = x : ys