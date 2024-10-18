{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day02 (
  parts,
) where

import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec (Parsec, many1, digit, char, optional, newline, runParser)
import Util.AOCHelpers (Part (Part), Parts)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Int
part1 input = sum $ zipWith (+) presentSurfaceAreas smallestSides
 where
  presentSurfaceAreas = surfaceArea <$> dimensionsList input
  smallestSides = smallestSide <$> dimensionsList input

part2 :: T.Text -> Int
part2 input = sum $ zipWith (+) wrapRibbonRequired bowRibbonRequired
 where
  wrapRibbonRequired = ribbonForWrap <$> dimensionsList input
  bowRibbonRequired = ribbonForBow <$> dimensionsList input

type PresentDimension = (Int, Int, Int)

parseDimensions :: Parsec T.Text () PresentDimension
parseDimensions = do 
  l <- many1 digit <* char 'x'
  w <- many1 digit <* char 'x'
  h <- many1 digit
  pure $ (read l, read w, read h)

parseDimensionsList :: Parsec T.Text () [PresentDimension]
parseDimensionsList = many1 $ parseDimensions <* optional newline

dimensionsList :: T.Text -> [PresentDimension]
dimensionsList input = either errorHandler id dimsList
  where
    dimsList = runParser parseDimensionsList () "parseDimensionsList" input
    errorHandler = error . show

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
