module AOC2015.Day09
  ( part1,
    part2,
  )
where

import Data.Foldable (minimumBy, maximumBy)
import Data.Function (on)
import Data.HashMap.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = fst $ minimumBy (compare `on` fst) $ filter ((== numCities) . length . snd) allPaths
  where
    allPaths = concat $ foldr (\(c1, c2) acc -> findPaths routes c1 c2 : acc) [] cityPairs
    cityPairs = [(c1, c2) | c1 <- M.keys routes, c2 <- M.keys routes]
    numCities = length $ M.keys routes
    routes = parseRoutes input

part2 :: T.Text -> Int
part2 input = fst $ maximumBy (compare `on` fst) $ filter ((== numCities) . length . snd) allPaths
  where
    allPaths = concat $ foldr (\(c1, c2) acc -> findPaths routes c1 c2 : acc) [] cityPairs
    cityPairs = [(c1, c2) | c1 <- M.keys routes, c2 <- M.keys routes]
    numCities = length $ M.keys routes
    routes = parseRoutes input

type Routes = M.HashMap String [(String, Int)]

type Path = (Int, [String])

findPaths :: Routes -> String -> String -> [Path]
findPaths routes start end = go S.empty (0, []) start
  where
    go visited (distance, p) current
      | current == end = [(distance, current : p)]
      | otherwise = concat $ mapMaybe buildPaths neighbors
      where
        neighbors = routes M.! current
        buildPaths (next, distToNext)
          | next `S.member` visited = Nothing
          | otherwise = Just $ go (S.insert current visited) (distance + distToNext, current : p) next

parseRoutes :: T.Text -> Routes
parseRoutes input = foldr addRouteFold M.empty routes
  where
    numParser = read <$> P.many1 P.digit
    cityParser = P.many1 P.letter
    routeParser =
      (,,)
        <$> cityParser
        <* P.string " to "
        <*> cityParser
        <* P.string " = "
        <*> numParser
    routesParser = P.many1 $ routeParser <* P.optional P.newline
    routes = parseAoCInput input routesParser "routesParser"
    putInMap (c1, c2, d) rs = M.insertWith (++) c1 [(c2, d)] (M.insertWith (++) c2 [(c1, d)] rs)
    addRouteFold (c1, c2, d) = putInMap (c1, c2, d)
