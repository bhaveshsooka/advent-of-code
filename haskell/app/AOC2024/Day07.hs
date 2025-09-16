module AOC2024.Day07
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (addIfValid False) 0 $ parseEquations input

part2 :: T.Text -> Int
part2 input = foldr (addIfValid True) 0 $ parseEquations input

type Equation = (Int, [Int])

addIfValid :: Bool -> Equation -> Int -> Int
addIfValid isPart2 e@(n, _) acc = if evalEquation isPart2 e 0 then acc + n else acc

evalEquation :: Bool -> Equation -> Int -> Bool
evalEquation _ (target, []) n = target == n
evalEquation isPart2 (target, x : xs) n =
  if isPart2
    then eval (n + x) || eval (n * x) || eval (concatInts n x)
    else eval (n + x) || eval (n * x)
  where
    concatInts a b = a * 10 ^ length (show (abs b)) + b
    eval = evalEquation isPart2 (target, xs)

parseEquations :: T.Text -> [Equation]
parseEquations input = parseAoCInput input equationsParser "equationsParser"
  where
    numParser = read <$> P.many1 P.digit
    argumentParser = P.sepBy1 numParser (P.char ' ')
    equationParser = (,) <$> numParser <* P.string ": " <*> argumentParser
    equationsParser = P.many1 $ equationParser <* P.optional P.newline
