module AOC2018.Day01
  ( part1,
    part2,
  )
where

import Data.Set qualified as S
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ parseFrequencies input

part2 :: T.Text -> Int
part2 input = findRepeatedFrequency S.empty 0 0 $ parseFrequencies input

findRepeatedFrequency :: (Num t, Ord t) => S.Set t -> t -> Int -> [t] -> t
findRepeatedFrequency seenFrequencies prevFrequency idx frequencies =
  if currentFrequency `S.member` seenFrequencies
    then currentFrequency
    else findRepeatedFrequency newSeenFrequencies currentFrequency newIdx frequencies
  where
    newSeenFrequencies = S.insert currentFrequency seenFrequencies
    newIdx = if (idx + 1) == length frequencies then 0 else idx + 1
    currentFrequency = prevFrequency + (frequencies !! idx)

parseFrequencies :: T.Text -> [Int]
parseFrequencies input = parseAoCInput input frequenciesParser "frequenciesParser"
  where
    frequencyParser = do
      sign <- P.choice $ P.try <$> [P.char '+', P.char '-']
      num <- read <$> P.many1 P.digit
      return $ if sign == '+' then num else -num
    frequenciesParser = P.many1 $ frequencyParser <* P.optional P.newline
