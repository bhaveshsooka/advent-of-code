module AOC2015.Day03
  ( part1,
    part2,
  )
where

import Data.List (nub, partition)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length . nub . scanl (<>) mempty $ getCoord <$> T.unpack input

part2 :: T.Text -> Int
part2 input = length . nub $ santa <> roboSanta
  where
    xx = partition (\(i :: Int, _) -> even i) $ zip [0 ..] $ getCoord <$> T.unpack input
    santa = scanl (<>) mempty $ snd <$> fst xx
    roboSanta = scanl (<>) mempty $ snd <$> snd xx

data Coord = Coord Int Int deriving (Show, Eq)

instance Semigroup Coord where
  (<>) :: Coord -> Coord -> Coord
  Coord x y <> Coord x' y' = Coord (x + x') (y + y')

instance Monoid Coord where
  mempty :: Coord
  mempty = Coord 0 0

getCoord :: Char -> Coord
getCoord c = case c of
  '^' -> Coord 0 1
  'v' -> Coord 0 (-1)
  '<' -> Coord (-1) 0
  '>' -> Coord 1 0
  _ -> error "Invalid direction"
