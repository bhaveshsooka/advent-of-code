module AOC2018.Day01
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ parseFrequencies input

part2 :: T.Text -> Int
part2 input = max x y
  where
    frequencies = parseFrequencies input
    partialSums = buildPartialSums frequencies (part1 input) 0 0 M.empty []
    (x, y, _) = case partialSums of
      [] -> (0, 0, 0)
      ((a, b, c) : restOfModPairs) -> findMins restOfModPairs (abs (a - b)) (a, b, c)

type PartialSumMod = Int

type PartialSum = Int

type PartialSumIdx = Int

type TrackingSums = M.Map PartialSumMod (PartialSum, PartialSumIdx)

type ModPair = (PartialSumMod, PartialSum, PartialSumIdx)

type ModPairs = [ModPair]

buildPartialSums :: [Int] -> Int -> Int -> PartialSumIdx -> TrackingSums -> ModPairs -> ModPairs
buildPartialSums data' p1 p2 i trackingSums modPairs
  | i == length data' = modPairs
  | partsMod - 1 /= partialSum1 = buildPartialSums data' p1 newPart2 (i + 1) newTrackingSums newModPairs
  | otherwise = buildPartialSums data' p1 newPart2 (i + 1) newTrackingSums modPairs
  where
    partsMod = p2 `mod` p1
    partialSum1 = maybe (partsMod - 1) fst (M.lookup partsMod trackingSums)
    newModPairs = modPairs ++ [(p2, fst $ trackingSums M.! partsMod, snd $ trackingSums M.! partsMod)]
    newTrackingSums = M.insert partsMod (p2, i) trackingSums
    newPart2 = p2 + (data' !! i)

findMins :: ModPairs -> Int -> ModPair -> ModPair
findMins [] _ min_difference_in_sums_pair = min_difference_in_sums_pair
findMins ((x, y, i) : modPairs) min_difference_in_sums min_difference_in_sums_pair =
  if number_of_fullsums_btwn_matched < min_difference_in_sums || (number_of_fullsums_btwn_matched == min_difference_in_sums && i < third min_difference_in_sums_pair)
    then findMins modPairs number_of_fullsums_btwn_matched (x, y, i)
    else findMins modPairs min_difference_in_sums min_difference_in_sums_pair
  where
    number_of_fullsums_btwn_matched = abs (x - y)
    third (_, _, c) = c

_recursiveSol :: [Int] -> [Int] -> Int -> Int
_recursiveSol _ [] acc = acc
_recursiveSol sums (x : xs) acc =
  if (acc + x) `elem` sums
    then _recursiveSol (sums ++ [acc + x]) [] (acc + x)
    else _recursiveSol (sums ++ [acc + x]) xs (acc + x)

parseFrequencies :: T.Text -> [Int]
parseFrequencies input = parseAoCInput input frequenciesParser "frequenciesParser"
  where
    frequencyParser = do
      sign <- P.choice $ P.try <$> [P.char '+', P.char '-']
      num <- read <$> P.many1 P.digit
      return $ if sign == '+' then num else -num
    frequenciesParser = P.many1 $ frequencyParser <* P.optional P.newline
