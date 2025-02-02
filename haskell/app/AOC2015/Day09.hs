module AOC2015.Day09
  ( part1,
    part2,
  )
where

import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = fst $ minimumBy (compare `on` fst) fullPaths
  where
    fullPaths = filter ((== length cities) . length . snd) allPaths
    allPaths =
      concat
        [ findPaths graph S.empty (0, [c1]) (c1, c2)
          | c1 <- cities,
            c2 <- cities,
            c1 /= c2
        ]
    cities = M.keys graph
    graph = parseRoutes input

part2 :: T.Text -> Int
part2 input = fst $ maximumBy (compare `on` fst) fullPaths
  where
    fullPaths = filter ((== length cities) . length . snd) allPaths
    allPaths =
      concat
        [ findPaths graph S.empty (0, [c1]) (c1, c2)
          | c1 <- cities,
            c2 <- cities,
            c1 /= c2
        ]
    cities = M.keys graph
    graph = parseRoutes input

type City = String

type Distance = Int

type Graph = M.HashMap City [(City, Distance)]

type Path = (Distance, [City])

type Visited = S.Set City

findPaths :: Graph -> Visited -> Path -> (City, City) -> [Path]
findPaths graph visited (dist, route) (node, end)
  | node == end = [(dist, route)]
  | otherwise = concat $ mapMaybe buildPaths neighbors
  where
    neighbors = graph M.! node
    buildPaths (neighbour, neighborDistance)
      | neighbour `S.member` visited = Nothing
      | otherwise = Just $ findPaths graph newVisited newPath (neighbour, end)
      where
        newVisited = S.insert node visited
        newPath = (dist + neighborDistance, neighbour : route)

parseRoutes :: T.Text -> Graph
parseRoutes input = foldr addRouteFold M.empty routes
  where
    numParser = read <$> P.many1 P.digit
    cityParser toVoid = P.many1 P.letter <* P.string toVoid
    routeParser = (,,) <$> cityParser " to " <*> cityParser " = " <*> numParser
    routesParser = P.many1 $ routeParser <* P.optional P.newline
    routes = parseAoCInput input routesParser "routesParser"
    putInMap (c1, c2, d) = M.insertWith (++) c1 [(c2, d)]
    addRouteFold (c1, c2, d) = putInMap (c1, c2, d) . putInMap (c2, c1, d)
