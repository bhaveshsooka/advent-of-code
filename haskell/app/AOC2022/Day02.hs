module AOC2022.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\(a, b) acc -> acc + roundScore P1 a b) 0 (parseRounds input)

part2 :: T.Text -> Int
part2 input = foldr (\(a, b) acc -> acc + roundScore P2 a b) 0 (parseRounds input)

data P = P1 | P2

roundScore :: P -> Char -> Char -> Int
roundScore p opp me = case (p, opp) of
  (P1, 'A') -> myScore + myLoseScore P2
  (P2, 'A') -> myScore + myWinScore P1
  (P1, 'B') -> myScore + myDrawScore P2
  (P2, 'B') -> myScore + myDrawScore P1
  (P1, 'C') -> myScore + myWinScore P2
  (P2, 'C') -> myScore + myLoseScore P1
  (_, _) -> 0
  where
    myScore = shapeScore p me
    myWinScore p' = shapeScore p' (whatIWin me)
    myLoseScore p' = shapeScore p' (whatILose me)
    myDrawScore p' = shapeScore p' (whatIDraw me)

whatIWin :: Char -> Char
whatIWin 'X' = 'C'
whatIWin 'Y' = 'A'
whatIWin 'Z' = 'B'
whatIWin _ = error "Invalid input"

whatIDraw :: Char -> Char
whatIDraw 'X' = 'A'
whatIDraw 'Y' = 'B'
whatIDraw 'Z' = 'C'
whatIDraw _ = error "Invalid input"

whatILose :: Char -> Char
whatILose 'X' = 'B'
whatILose 'Y' = 'C'
whatILose 'Z' = 'A'
whatILose _ = error "Invalid input"

shapeScore :: P -> Char -> Int
shapeScore p shape = case p of
  P1 -> shapeScore' shape
  P2 -> (shapeScore' shape - 1) * 3
  where
    shapeScore' s
      | s == 'X' || s == 'A' = 1
      | s == 'Y' || s == 'B' = 2
      | s == 'Z' || s == 'C' = 3
      | otherwise = 0

parseRounds :: T.Text -> [(Char, Char)]
parseRounds input = parseAoCInput input roundsParser "roundsParser"
  where
    charParser = P.anyChar
    roundParser = (,) <$> charParser <* P.char ' ' <*> charParser
    roundsParser = P.many1 $ roundParser <* P.optional P.newline
