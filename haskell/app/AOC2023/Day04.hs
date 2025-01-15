module AOC2023.Day04
  ( part1,
    part2,
  )
where

import Data.HashMap.Strict qualified as M
import Data.List (intersect)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ getPoints <$> scratchCards
  where
    getPoints (ScratchCard _ wns ns) = case wns `intersect` ns of
      [] -> 0
      xs -> 2 ^ (length xs - 1)
    scratchCards = parseScratchCards input

part2 :: T.Text -> Int
part2 input = sum cardCounts
  where
    cardCounts = foldl totalScratchCardsFold initalScratchCardsCount scratchCards
    initalScratchCardsCount = M.fromList $ (\(ScratchCard cardId _ _) -> (cardId, 1)) <$> scratchCards
    scratchCards = parseScratchCards input

data ScratchCard = ScratchCard Int [Int] [Int] deriving (Show)

type ScratchCards = [ScratchCard]

type ScratchCardsCount = M.HashMap Int Int

totalScratchCardsFold :: ScratchCardsCount -> ScratchCard -> ScratchCardsCount
totalScratchCardsFold scCounts (ScratchCard cardId wns ns) =
  case scCounts M.!? cardId of
    Nothing -> scCounts
    Just cardCount -> foldr countWinningNumbersFold scCounts [0 .. cardCount - 1]
  where
    countWinningNumbersFold _ acc = foldr updateScratchCardsCountFold acc [1 .. length $ wns `intersect` ns]
    updateScratchCardsCountFold j = M.insertWith (+) (cardId + j) 1

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
