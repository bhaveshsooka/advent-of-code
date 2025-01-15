module AOC2015.Day03
  ( part1,
    part2,
  )
where

import Data.List (nub, partition)
import Data.Text qualified as T
import Util.GridUtils.Coord (Coord (Coord))

part1 :: T.Text -> Int
part1 input = length . nub . scanl (<>) mempty $ getCoord <$> T.unpack input

part2 :: T.Text -> Int
part2 input = length . nub $ santa <> roboSanta
  where
    xx = partition (even . fst) $ zip [0 :: Int ..] $ getCoord <$> T.unpack input
    santa = scanl (<>) mempty $ snd <$> fst xx
    roboSanta = scanl (<>) mempty $ snd <$> snd xx

getCoord :: Char -> Coord
getCoord c = case c of
  '^' -> Coord 0 1
  'v' -> Coord 0 (-1)
  '<' -> Coord (-1) 0
  '>' -> Coord 1 0
  _ -> error "Invalid direction"
