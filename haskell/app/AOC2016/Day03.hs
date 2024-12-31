module AOC2016.Day03
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = length $ filter isValidTriangle $ parseTriangles input

part2 :: T.Text -> Int
part2 input = verticalTriangles 0 $ parseTriangles input

data Triangle = Triangle Int Int Int deriving (Show)

verticalTriangles :: Int -> [Triangle] -> Int
verticalTriangles acc [] = acc
verticalTriangles acc [_] = acc
verticalTriangles acc [_, _] = acc
verticalTriangles acc (Triangle a1 b1 c1 : Triangle a2 b2 c2 : Triangle a3 b3 c3 : rest) =
  verticalTriangles (acc + length validNewTriangles) rest
  where
    validNewTriangles = filter isValidTriangle newTriangles
    newTriangles = [Triangle a1 a2 a3, Triangle b1 b2 b3, Triangle c1 c2 c3]

isValidTriangle :: Triangle -> Bool
isValidTriangle (Triangle a b c) = a + b > c && a + c > b && b + c > a

parseTriangles :: T.Text -> [Triangle]
parseTriangles input = parseAoCInput input trianglesParser "trianglesParser"
  where
    spaceParser = P.many1 (P.string " ")
    sideParser = read <$> (spaceParser *> P.many1 P.digit)
    triangleParser = Triangle <$> sideParser <*> sideParser <*> sideParser
    trianglesParser = P.many1 $ triangleParser <* P.optional P.newline
