module AOC2015.Day14
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = maximum $ calcDistance 2503 <$> parseReindeerStats input

part2 :: T.Text -> Int
part2 input = maximum $ foldr (zipWith (+)) startingPoints positions
  where
    startingPoints = replicate (length reindeerStats) 0
    positions = winners <$> distances
    winners arr = (\e -> if e == maximum arr then 1 else 0) <$> arr
    distances = (\n -> calcDistance n <$> reindeerStats) <$> [1 .. 2503]
    reindeerStats = parseReindeerStats input

type Seconds = Int

type Distance = Int

type Name = String

type Velocity = Int

type FlyTime = Int

type RestTime = Int

type ReindeerStat = (Name, Velocity, FlyTime, RestTime)

type ReindeerStats = [ReindeerStat]

calcDistance :: Seconds -> ReindeerStat -> Distance
calcDistance n (_, v, ft, rt) = go 0 0
  where
    isMoving s = (s `mod` (ft + rt)) < ft
    go s d
      | s == n = d
      | isMoving s = go (s + 1) (d + v)
      | otherwise = go (s + 1) d

parseReindeerStats :: T.Text -> ReindeerStats
parseReindeerStats input = parseAoCInput input reindeerStatsParser "reindeerStatParser"
  where
    numParser = read <$> P.many1 P.digit
    nameParser = P.many1 $ P.noneOf [' ']
    reindeerStatParser =
      (,,,)
        <$> (nameParser <* P.string " can fly ")
        <*> (numParser <* P.string " km/s for ")
        <*> (numParser <* P.string " seconds, but then must rest for ")
        <*> (numParser <* P.string " seconds.")
    reindeerStatsParser = P.many1 $ reindeerStatParser <* P.optional P.newline
