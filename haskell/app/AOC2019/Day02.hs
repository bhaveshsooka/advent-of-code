module AOC2019.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input =
  case processProgram preparedProgram 0 of
    [] -> error "no program"
    (x : _) -> x
  where
    preparedProgram = take 1 program ++ [noun, verb] ++ drop 3 program
    noun = 12
    verb = 2
    program = parseProgram input

part2 :: T.Text -> Int
part2 input = findCorrectProgram allThePrograms target
  where
    target :: Int = 19690720
    allThePrograms = [take 1 program ++ [noun, verb] ++ drop 3 program | noun <- [0 .. 99], verb <- [0 .. 99]]
    program = parseProgram input

findCorrectProgram :: [[Int]] -> Int -> Int
findCorrectProgram [] _ = -1
findCorrectProgram (p : ps) target =
  if progOutput == target
    then 100 * noun + verb
    else findCorrectProgram ps target
  where
    progOutput = case processProgram p 0 of
      [] -> -1
      (x : _) -> x
    noun = p !! 1
    verb = p !! 2

parseProgram :: T.Text -> [Int]
parseProgram input = parseAoCInput input programParser "programParser"
  where
    numParser = read <$> P.many1 P.digit
    instructionParser = numParser <* P.optional (P.char ',')
    programParser = P.many1 instructionParser <* P.optional P.newline

processProgram :: [Int] -> Int -> [Int]
processProgram prog i =
  if i >= length prog
    then prog
    else case opcode of
      1 -> processProgram (take updatePos prog ++ [a + b] ++ drop (updatePos + 1) prog) (i + 4)
      2 -> processProgram (take updatePos prog ++ [a * b] ++ drop (updatePos + 1) prog) (i + 4)
      99 -> prog
      _ -> error "o-ops"
  where
    opcode = prog !! i
    aPos = prog !! (i + 1)
    a = prog !! aPos
    bPos = prog !! (i + 2)
    b = prog !! bPos
    updatePos = prog !! (i + 3)
