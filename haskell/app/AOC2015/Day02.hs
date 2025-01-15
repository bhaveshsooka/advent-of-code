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
    surfaceArea (Box l w h) = 2 * (l * w + w * h + l * h)
    smallestSide (Box l w h) = minimum [l * w, w * h, h * l]

part2 :: T.Text -> Int
part2 input = foldr (\b acc -> acc + ribbonForWrap b + ribbonForBow b) 0 $ parseDimensionsList input
  where
    ribbonForBow (Box l w h) = l * w * h
    ribbonForWrap (Box l w h) = (2 * minimum [l, w, h]) + (2 * sort [l, w, h] !! 1)

data PresentDimension = Box Int Int Int

parseDimensionsList :: T.Text -> [PresentDimension]
parseDimensionsList input = parseAoCInput input dimensionsListParser "dimensionsListParser"
  where
    numParser = read <$> P.many1 P.digit
    dimensionsParser = Box <$> numParser <* P.char 'x' <*> numParser <* P.char 'x' <*> numParser
    dimensionsListParser = P.many1 $ dimensionsParser <* P.optional P.newline
