module AOC2016.Day06
  ( part1,
    part2,
  )
where

import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.Map qualified as M
import Data.Text qualified as T

part1 :: T.Text -> String
part1 input = commonFreqsString columnFreqs
  where
    columnFreqs = maximumBy (compare `on` snd) . M.toList . countFrequencies <$> parseColumns input

part2 :: T.Text -> String
part2 input = commonFreqsString columnFreqs
  where
    columnFreqs = minimumBy (compare `on` snd) . M.toList . countFrequencies <$> parseColumns input

type Columns = M.Map Int String

commonFreqsString :: M.Map Int (Char, Int) -> String
commonFreqsString = M.foldr (\(c, _) acc -> c : acc) ""

countFrequencies :: String -> M.Map Char Int
countFrequencies = foldr (\c acc -> M.insertWith (+) c 1 acc) M.empty

parseColumns :: T.Text -> Columns
parseColumns input = foldr foldFn M.empty $ lines (T.unpack input)
  where
    foldFn x acc = foldr foldCols acc $ zip [0 ..] x
    foldCols (i, c) = M.insertWith (flip (++)) i [c]
