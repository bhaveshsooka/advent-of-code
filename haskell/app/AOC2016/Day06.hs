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
    columnFreqs = M.map (maximumBy (compare `on` snd) . M.toList . countFrequencies) $ parseColumns input

part2 :: T.Text -> String
part2 input = commonFreqsString columnFreqs
  where
    columnFreqs = M.map (minimumBy (compare `on` snd) . M.toList . countFrequencies) $ parseColumns input

type Columns = M.Map Int String

commonFreqsString :: M.Map Int (Char, Int) -> String
commonFreqsString = M.foldl (\acc (c, _) -> acc <> [c]) ""

countFrequencies :: String -> M.Map Char Int
countFrequencies = foldr (\c acc -> M.insertWith (+) c 1 acc) M.empty

parseColumns :: T.Text -> Columns
parseColumns = go M.empty . T.lines
  where
    go :: Columns -> [T.Text] -> Columns
    go cols [] = cols
    go cols (x : xs) = go newCols xs
      where
        indexes = zip [0 ..] (T.unpack x)
        newCols = foldr (\(i, c) acc -> M.insertWith (flip (++)) i [c] acc) cols indexes
