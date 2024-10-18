module AOC2015.Day03 (
  parts,
) where

import Data.List (nub)
import Data.Text qualified as T
import Util.AOCHelpers (Part (Part), Parts)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = length $ nub $ scanAndTake 1 allCoords
 where
  allCoords = getCoords input

part2 :: T.Text -> Int
part2 input = length $ nub $ santa ++ roboSanta
 where
  allCoords = getCoords input
  santa = scanAndTake 2 allCoords
  roboSanta = scanAndTake 2 $ mempty : allCoords

data Coord = Coord Int Int deriving (Show, Eq)

instance Semigroup Coord where
  (<>) :: Coord -> Coord -> Coord
  Coord x y <> Coord x' y' = Coord (x + x') (y + y')

instance Monoid Coord where
  mempty :: Coord
  mempty = Coord 0 0

scanAndTake :: (Monoid a) => Int -> [a] -> [a]
scanAndTake n = scanl (<>) mempty . takeEvery n

getCoord :: Char -> Coord
getCoord c = case c of
  '^' -> Coord 0 1
  'v' -> Coord 0 (-1)
  '<' -> Coord (-1) 0
  '>' -> Coord 1 0
  _ -> error "Invalid direction"

getCoords :: T.Text -> [Coord]
getCoords input = getCoord <$> T.unpack input

takeEvery :: Int -> [a] -> [a]
takeEvery step xs = compute validated
 where
  compute ys = case drop (step - 1) ys of
    [] -> []
    y : ys' -> y : compute ys'
  validated =
    if step > 0
      then xs
      else error "Data.List.Transform.takeEvery: Step parameter must be positive."
