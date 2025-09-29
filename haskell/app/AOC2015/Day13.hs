module AOC2015.Day13
  ( part1,
    part2,
  )
where

import Data.HashMap.Internal.Strict qualified as H
import Data.List (find, permutations)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = maximum $ totalHappiness rules <$> seatingArrangements
  where
    rules = parseHappinessRules input
    seatingArrangements = cyclicPerms $ H.keys rules

part2 :: T.Text -> Int
part2 input = maximum $ totalHappiness newRules <$> seatingArrangements
  where
    rules = parseHappinessRules input
    myRules = (,0) <$> H.keys rules
    newRules = H.insert "It's a me mario" myRules rules
    seatingArrangements = cyclicPerms $ H.keys newRules

cyclicPerms :: [a] -> [[a]]
cyclicPerms [] = [[]]
cyclicPerms (x : xs) = map (x :) (permutations xs)

type Name = String

type HappinessLevel = Int

type Rule = (Name, HappinessLevel)

type HappinessRules = H.HashMap Name [Rule]

totalHappiness :: HappinessRules -> [Name] -> Int
totalHappiness rules table = foldr addMyHappiness 0 indexedTable
  where
    indexedTable = zip [0 ..] table
    addMyHappiness (i, me) acc = acc + getHappiness me (leftOfMe i) + getHappiness me (rightOfMe i)
    myRules me = rules H.! me
    leftOfMe idx = table !! mod (idx - 1) (length table)
    rightOfMe idx = table !! mod (idx + 1) (length table)
    getHappiness me neighbour =
      case find ((==) neighbour . fst) (myRules me) of
        Just (_, h) -> h
        Nothing -> 0

parseHappinessRules :: T.Text -> HappinessRules
parseHappinessRules input = foldr insertRule H.empty happinessRulesList
  where
    numParser = read <$> P.many1 P.digit
    nameParser = P.many $ P.noneOf [' ', '.']
    gainParser = P.try (P.string " would gain " *> numParser)
    loseParser = P.try $ (* (-1)) <$> (P.string " would lose " *> numParser)
    bloat = P.string " happiness units by sitting next to "
    happinessLevelParser = P.choice [gainParser, loseParser] <* bloat
    happinessRuleParser = (,,) <$> nameParser <*> happinessLevelParser <*> (nameParser <* P.char '.')
    happinessRulesParser = P.many1 $ happinessRuleParser <* P.optional P.newline
    happinessRulesList = parseAoCInput input happinessRulesParser "happinessRulesParser"
    insertRule (name, level, nextTo) = H.insertWith (++) name [(nextTo, level)]
