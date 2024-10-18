{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day02 (
  parts,
) where

import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec (Parsec, char, digit, many1, newline, optional)
import Util.AOCHelpers (Part (Part), Parts, parseAoCInput)

parts :: Parts
parts = (Part part1, Part part2)

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
    l <- many1 digit <* char 'x'
    w <- many1 digit <* char 'x'
    h <- many1 digit
    pure $ (read l, read w, read h)
  dimensionsListParser = many1 $ dimensionsParser <* optional newline

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
