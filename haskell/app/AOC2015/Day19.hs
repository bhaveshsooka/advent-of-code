module AOC2015.Day19
  ( part1,
    part2,
  )
where

import Data.HashSet qualified as Set
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = Set.size $ Set.fromList $ concat [doReplacements pat rep molecule | (pat, rep) <- rpls]
  where
    (rpls, molecule) = parseInput input

part2 :: T.Text -> Int
part2 input = fabricationSteps repls "e" medicine
  where
    (repls, medicine) = parseInput input

type Replacements = [(String, String)]

fabricationSteps :: [(String, String)] -> String -> String -> Int
fabricationSteps repls start end = fromJust $ go 0 end
  where
    nub' = Set.toList . Set.fromList
    replsO = sortOn (Down . length . snd) repls
    go steps cur
      | cur == start = Just steps
      | otherwise = listToMaybe $ mapMaybe (go (steps + 1)) reductions
      where
        reductions = nub' $ concat [doReplacements pat rep cur | (rep, pat) <- replsO]

doReplacements :: String -> String -> String -> [String]
doReplacements pat rep src = go [] src []
  where
    n = length pat
    go _ [] acc = acc
    go pre rest acc =
      go
        (pre ++ take 1 rest)
        (drop 1 rest)
        (if pat `isPrefixOf` rest then (pre ++ rep ++ drop n rest) : acc else acc)

parseInput :: T.Text -> (Replacements, String)
parseInput input = parseAoCInput input inputParser "AOC2015.Day19.parser"
  where
    replacementParser = (,) <$> P.many1 P.letter <* P.string " => " <*> P.many1 P.letter
    replacementsParser = P.many1 (replacementParser <* P.newline)
    moleculeParser = P.newline *> P.many1 P.letter <* P.optional P.newline
    inputParser = (,) <$> replacementsParser <*> moleculeParser
