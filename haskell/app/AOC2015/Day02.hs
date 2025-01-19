module AOC2015.Day02
  ( part1,
    part2,
  )
where

import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\b acc -> acc + surfaceArea b + smallestSide b) 0 $ parseDimensionsList input
  where
    surfaceArea (l, w, h) = 2 * (l * w + w * h + l * h)
    smallestSide (l, w, h) = minimum [l * w, w * h, h * l]

part2 :: T.Text -> Int
part2 input = foldr (\b acc -> acc + ribbonForWrap b + ribbonForBow b) 0 $ parseDimensionsList input
  where
    ribbonForBow (l, w, h) = l * w * h
    ribbonForWrap (l, w, h) = (2 * minimum [l, w, h]) + (2 * sort [l, w, h] !! 1)

parseDimensionsList :: T.Text -> [(Int, Int, Int)]
parseDimensionsList input = parseAoCInput input dimensionsListParser "dimensionsListParser"
  where
    numParser = read <$> P.many1 P.digit
    dimensionsParser = (,,) <$> numParser <* P.char 'x' <*> numParser <* P.char 'x' <*> numParser
    dimensionsListParser = P.many1 $ dimensionsParser <* P.optional P.newline
