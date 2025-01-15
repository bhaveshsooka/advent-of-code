module AOC2015.Day06
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector.Unboxed qualified as V
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Block, Coord (..))
import Util.GridUtils.Grid (UnboxedGrid)
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = V.sum $ foldl (processInstruction False) emptyGrid $ parseInstructions input

part2 :: T.Text -> Int
part2 input = V.sum $ foldl (processInstruction True) emptyGrid $ parseInstructions input

data Instruction = TurnOn Block | TurnOff Block | Toggle Block deriving (Show, Eq)

emptyGrid :: UnboxedGrid Int
emptyGrid = V.generate (1000 * 1000) $ const 0

parseInstructions :: T.Text -> [Instruction]
parseInstructions input = parseAoCInput input instructionsParser "instructionsParser"
  where
    numParser = read <$> P.many1 P.digit
    coordParser = Coord <$> (numParser <* P.char ',') <*> numParser
    blockParser = (,) <$> (coordParser <* P.string " through ") <*> coordParser
    turnOnParser = TurnOn <$> (P.string "turn on " *> blockParser)
    turnOffParser = TurnOff <$> (P.string "turn off " *> blockParser)
    toggleParser = Toggle <$> (P.string "toggle " *> blockParser)
    instructionParser = P.choice $ P.try <$> [turnOnParser, turnOffParser, toggleParser]
    instructionsParser = P.many1 $ instructionParser <* P.optional P.newline

processInstruction :: Bool -> UnboxedGrid Int -> Instruction -> UnboxedGrid Int
processInstruction b grid i =
  case (i, b) of
    (TurnOn block, False) -> grid V.// newVals block (const 1)
    (TurnOff block, False) -> grid V.// newVals block (const 0)
    (Toggle block, False) -> grid V.// newVals block (\v -> if v == 0 then 1 else 0)
    (TurnOn block, True) -> grid V.// newVals block (+ 1)
    (TurnOff block, True) -> grid V.// newVals block (\v -> if v == 0 then 0 else v - 1)
    (Toggle block, True) -> grid V.// newVals block (+ 2)
  where
    idxAndVal x y fn = (x * 1000 + y, fn $ grid V.! (x * 1000 + y))
    newVals (Coord x1 y1, Coord x2 y2) fn = [idxAndVal x y fn | x <- [x1 .. x2], y <- [y1 .. y2]]
