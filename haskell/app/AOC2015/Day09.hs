module AOC2015.Day09 (
  part1,
  part2,
) where

import Data.Map (Map, empty, insert, member, (!))
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> String
part1 _input = "Not there yet"

part2 :: T.Text -> String
part2 _input = "Not there yet"

type City = String
type Distance = Int
type Edge = (City, City)
type G = Map City [(City, Distance)]

_parseEdges :: T.Text -> [(Edge, Distance)]
_parseEdges input = parseAoCInput input edgesParser "edgesParser"
 where
  edgeParser = do
    city1 <- P.many1 P.letter <* P.string " to "
    city2 <- P.many1 P.letter <* P.string " = "
    distance <- read <$> P.many1 P.digit
    return ((city1, city2), distance)
  edgesParser = P.many1 $ edgeParser <* P.optional P.newline

_createGraph :: [(Edge, Distance)] -> G
_createGraph = foldr (\(a, b) g -> _insertEdge a b g) empty

_insertEdge :: Edge -> Distance -> G -> G
_insertEdge (a, b) d g
  | not $ member a g = insert a [(b, d)] g
  | otherwise = insert a ((b, d) : filter ((/= b) . fst) (g ! a)) g
