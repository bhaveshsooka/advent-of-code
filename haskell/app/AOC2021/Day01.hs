module AOC2021.Day01
  ( part1,
    part2,
  )
where

import Data.List (tails)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = fst $ foldr isDeeper (0, 0) $ parseDepths input
  where
    isDeeper e (acc, pe) = if e < pe then (acc + 1, e) else (acc, e)

part2 :: T.Text -> Int
part2 input = fst $ foldr (isDeeper . sum) (0, 0) windowedDepths
  where
    isDeeper e (acc, pe) = if e < pe then (acc + 1, e) else (acc, e)
    windowedDepths = foldr (zipWith (:)) (repeat []) . take 3 . tails $ parseDepths input

parseDepths :: T.Text -> [Int]
parseDepths input = parseAoCInput input depthsParser "depthsParser"
  where
    depthParser = read <$> P.many1 P.digit
    depthsParser = P.many1 $ depthParser <* P.optional P.newline
