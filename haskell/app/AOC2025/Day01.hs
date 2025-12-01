module AOC2025.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Debug.Trace (trace)
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = snd $ count0s (parseRotations input) (0, 50)

-- part2 :: T.Text -> Int
part2 input = countClicks (parseRotations input) (0, 50)

data Rotation = L Int | R Int deriving (Show)

countClicks :: [Rotation] -> (Int, Int) -> (Int, Int)
countClicks [] acc = acc
countClicks (x : xs) (count, currentVal) =
  trace (show (newVal, newVal `mod` 100, newVal `div` 100) ) $
  if (newVal `div` 100) /= 0
    then countClicks xs (count + abs (newVal `div` 100), newVal `mod` 100)
    else countClicks xs (count, newVal `mod` 100)
  where
    newVal = case x of L rv -> currentVal - rv; R rv -> currentVal + rv

count0s :: [Rotation] -> (Int, Int) -> (Int, Int)
count0s [] acc = acc
count0s (x : xs) (count, currentVal) =
  if newVal == 0
    then count0s xs (count + 1, newVal)
    else count0s xs (count, newVal)
  where
    newVal = case x of L rv -> (currentVal - rv) `mod` 100; R rv -> (currentVal + rv) `mod` 100

parseRotations :: T.Text -> [Rotation]
parseRotations input = parseAoCInput input rotationsParser "parseRotations"
  where
    numParser = read <$> P.many1 P.digit
    leftParser = L <$> (P.char 'L' *> numParser)
    rightParser = R <$> (P.char 'R' *> numParser)
    rotationParser = P.choice $ P.try <$> [leftParser, rightParser]
    rotationsParser = P.many1 $ rotationParser <* P.optional P.newline
