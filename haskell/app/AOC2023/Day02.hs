module AOC2023.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\(Game n _) acc -> acc + n) 0 possibleGames
  where
    games = parseGames input

    isRoundPossible [] = True
    isRoundPossible (c : rest) = case c of
      Red n -> n <= 12 && isRoundPossible rest
      Green n -> n <= 13 && isRoundPossible rest
      Blue n -> n <= 14 && isRoundPossible rest

    possibleGames = filter (\(Game _ rounds) -> all isRoundPossible rounds) games

part2 :: T.Text -> Int
part2 input = foldr ((\(r, g, b) acc -> acc + (r * g * b)) . minForGame) 0 games
  where
    games = parseGames input

    maxColoursInRound acc [] = acc
    maxColoursInRound (r, g, b) (c : cs) = case c of
      Red n -> maxColoursInRound (max n r, g, b) cs
      Green n -> maxColoursInRound (r, max n g, b) cs
      Blue n -> maxColoursInRound (r, g, max n b) cs

    minForGame (Game _ rounds) = foldl maxColoursInRound (0, 0, 0) rounds

data Colour
  = Red Int
  | Green Int
  | Blue Int
  deriving (Show, Eq)

type Round = [Colour]

type Rounds = [Round]

data Game = Game Int Rounds deriving (Show, Eq)

parseGames :: T.Text -> [Game]
parseGames input = parseAoCInput input gamesParser "gamesParser"
  where
    numParser = read <$> P.many1 P.digit
    cParser c = case c of
      Red _ -> P.try (Red <$> numParser <* P.string " red")
      Green _ -> P.try (Green <$> numParser <* P.string " green")
      Blue _ -> P.try (Blue <$> numParser <* P.string " blue")
    colourParser = P.choice [cParser (Red 0), cParser (Blue 0), cParser (Green 0)]
    roundParser = P.sepBy1 colourParser (P.string ", ")
    roundsParser = P.sepBy1 roundParser (P.string "; ")
    gameIdParser = P.string "Game " *> numParser <* P.string ": "
    gameParser = Game <$> gameIdParser <*> roundsParser
    gamesParser = P.many1 $ gameParser <* P.optional P.newline
