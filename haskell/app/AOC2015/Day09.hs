{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day09 (
  parts,
) where

import Data.Map (Map, empty, insert, member, (!))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Parsec qualified as P
import Util.AOCHelpers (Part (Part), Parts, parseAoCInput)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> String
part1 _input = "Not there yet"

part2 :: T.Text -> String
part2 _input = "Not there yet"

type City = String
type Distance = Int
type Edge = (City, City)
type G = Map City ([(City, Distance)])

parseEdges :: T.Text -> [(Edge, Distance)]
parseEdges input = parseAoCInput input edgesParser "edgesParser"
 where
  edgeParser = do
    city1 <- P.many1 P.letter <* P.string " to "
    city2 <- P.many1 P.letter <* P.string " = "
    distance <- read <$> P.many1 P.digit
    return ((city1, city2), distance)
  edgesParser = P.many1 $ edgeParser <* P.optional P.newline

createGraph :: [(Edge, Distance)] -> G
createGraph = foldr (\(a, b) g -> insertEdge a b g) empty

insertEdge :: Edge -> Distance -> G -> G
insertEdge (a, b) d g
  | not $ member a g = insert a [(b, d)] g
  | otherwise = insert a ((b, d) : filter ((/= b) . fst) (g ! a)) g
