module AOC2016.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)
import Prelude hiding (Left, Right)

part1 :: T.Text -> Int
part1 input = manhattanDistance start end
  where
    end = fst $ last path
    manhattanDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)
    path = foldl foldFn [(start, startDir)] turns
    foldFn acc turn = acc ++ drop 1 (processTurn (last acc) turn)
    (start, startDir) = (Coord 0 0, Up)
    turns = parseTurns input

part2 :: T.Text -> Int
part2 input = manhattanDistance start end
  where
    end = findRepeat $ fst <$> path
    manhattanDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)
    path = foldl foldFn [(start, startDir)] turns
    foldFn acc turn = acc ++ drop 1 (processTurn (last acc) turn)
    (start, startDir) = (Coord 0 0, Up)
    turns = parseTurns input

data Turn = R Int | L Int deriving (Show, Eq)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

data Coord = Coord Int Int deriving (Show, Eq)

findRepeat :: [Coord] -> Coord
findRepeat [] = error "no repeat"
findRepeat (x : xs) =
  if x `elem` xs
    then x
    else findRepeat xs

processTurn :: (Coord, Direction) -> Turn -> [(Coord, Direction)]
processTurn (Coord x y, d) turn = case (d, turn) of
  (Up, R n) -> [(Coord x (y + i), Right) | i <- [0 .. n]]
  (Up, L n) -> [(Coord x (y - i), Left) | i <- [0 .. n]]
  (Down, R n) -> [(Coord x (y - i), Left) | i <- [0 .. n]]
  (Down, L n) -> [(Coord x (y + i), Right) | i <- [0 .. n]]
  (Left, R n) -> [(Coord (x - i) y, Up) | i <- [0 .. n]]
  (Left, L n) -> [(Coord (x + i) y, Down) | i <- [0 .. n]]
  (Right, R n) -> [(Coord (x + i) y, Down) | i <- [0 .. n]]
  (Right, L n) -> [(Coord (x - i) y, Up) | i <- [0 .. n]]

parseTurns :: T.Text -> [Turn]
parseTurns input = parseAoCInput input turnsParser "turnsParser"
  where
    numParser = read <$> P.many1 P.digit
    turnParser = R <$> (P.string "R" *> numParser) P.<|> L <$> (P.string "L" *> numParser)
    turnsParser = P.many1 (turnParser <* P.optional (P.string ", "))
