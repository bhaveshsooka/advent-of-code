module AOC2015.Day02 (
  part1,
  part2,
) where

import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ zipWith (+) presentSurfaceAreas smallestSides
 where
  presentSurfaceAreas = surfaceArea <$> parseDimensionsList input
  smallestSides = smallestSide <$> parseDimensionsList input

part2 :: T.Text -> Int
part2 input = sum $ zipWith (+) wrapRibbonRequired bowRibbonRequired
 where
  wrapRibbonRequired = ribbonForWrap <$> parseDimensionsList input
  bowRibbonRequired = ribbonForBow <$> parseDimensionsList input

type PresentDimension = (Int, Int, Int)

parseDimensionsList :: T.Text -> [PresentDimension]
parseDimensionsList input = parseAoCInput input dimensionsListParser "dimensionsListParser"
 where
  dimensionsParser = do
    l <- P.many1 P.digit <* P.char 'x'
    w <- P.many1 P.digit <* P.char 'x'
    h <- P.many1 P.digit
    pure $ (read l, read w, read h)
  dimensionsListParser = P.many1 $ dimensionsParser <* P.optional P.newline

area :: Int -> Int -> Int
area l w = l * w

surfaceArea :: PresentDimension -> Int
surfaceArea (l, w, h) = 2 * (area l w + area w h + area l h)

smallestSide :: PresentDimension -> Int
smallestSide (l, w, h) = min (area l w) (min (area w h) (area h l))

ribbonForBow :: PresentDimension -> Int
ribbonForBow (l, w, h) = l * w * h

ribbonForWrap :: PresentDimension -> Int
ribbonForWrap (l, w, h) = (2 * sort [l, w, h] !! 0) + (2 * sort [l, w, h] !! 1)
