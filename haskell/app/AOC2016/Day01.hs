module AOC2016.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Coord (Coord), manhattanDistance)
import Util.GridUtils.DirectionVonNeumann (Direction (..))
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = manhattanDistance (Coord 0 0) $ fst $ last path
  where
    path = foldl pathBuilder [(Coord 0 0, N)] $ parseTurns input

part2 :: T.Text -> Int
part2 input = manhattanDistance (Coord 0 0) end
  where
    end = findRepeat $ fst <$> path
    path = foldl pathBuilder [(Coord 0 0, N)] $ parseTurns input

    findRepeat [] = error "no repeat"
    findRepeat (x : xs) = if x `elem` xs then x else findRepeat xs

data Turn = R Int | L Int deriving (Show, Eq)

type Turns = [Turn]

type PathElem = (Coord, Direction)

type Path = [PathElem]

pathBuilder :: Path -> Turn -> Path
pathBuilder acc turn = acc ++ drop 1 (turnAndStep (last acc))
  where
    turnAndStep (Coord x y, d) = case (d, turn) of
      (N, R n) -> [(Coord x (y + i), E) | i <- [0 .. n]]
      (N, L n) -> [(Coord x (y - i), W) | i <- [0 .. n]]
      (E, R n) -> [(Coord (x + i) y, S) | i <- [0 .. n]]
      (E, L n) -> [(Coord (x - i) y, N) | i <- [0 .. n]]
      (S, R n) -> [(Coord x (y - i), W) | i <- [0 .. n]]
      (S, L n) -> [(Coord x (y + i), E) | i <- [0 .. n]]
      (W, R n) -> [(Coord (x - i) y, N) | i <- [0 .. n]]
      (W, L n) -> [(Coord (x + i) y, S) | i <- [0 .. n]]

parseTurns :: T.Text -> Turns
parseTurns input = parseAoCInput input turnsParser "turnsParser"
  where
    numParser = read <$> P.many1 P.digit
    rParser = R <$> (P.string "R" *> numParser)
    lParser = L <$> (P.string "L" *> numParser)
    turnParser = P.choice $ P.try <$> [rParser, lParser]
    turnsParser = P.many1 $ turnParser <* P.optional (P.string ", ")
