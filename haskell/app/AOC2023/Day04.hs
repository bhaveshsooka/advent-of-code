module AOC2023.Day04
  ( part1,
    part2,
  )
where

import Data.List (intersect)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ countPoints <$> scratchCards
  where
    countPoints c = if countWins c == 0 then 0 else 2 ^ (countWins c - 1)
    scratchCards = parseScratchCards input

part2 :: T.Text -> Int
part2 input = sum $ foldr (countCards . countWins) [] scratchCards
  where
    countCards w xs = 1 + sum (take w xs) : xs
    scratchCards = parseScratchCards input

data ScratchCard = ScratchCard Int [Int] [Int] deriving (Show)

type ScratchCards = [ScratchCard]

countWins :: ScratchCard -> Int
countWins (ScratchCard _ wns ns) = length $ wns `intersect` ns

parseScratchCards :: T.Text -> ScratchCards
parseScratchCards input = parseAoCInput input scratchCardsParser "scratchCardsParser"
  where
    numParser = read <$> P.many1 P.digit
    spacesParser = P.many1 (P.string " ")
    winningNumbersString = spacesParser *> P.many1 (numParser <* spacesParser) <* P.optional (P.string "|")
    numbersListParser = spacesParser *> P.sepBy1 numParser spacesParser
    scratchCardIdParser = P.string "Card" *> spacesParser *> numParser <* P.string ":"
    scratchCardParser = ScratchCard <$> scratchCardIdParser <*> winningNumbersString <*> numbersListParser
    scratchCardsParser = P.many1 $ scratchCardParser <* P.optional P.newline
