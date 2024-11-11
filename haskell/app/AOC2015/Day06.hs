module AOC2015.Day06
  ( part1,
    part2,
  )
where

import Data.Functor (($>))
import Data.Text qualified as T
import Data.Vector.Unboxed (Vector, filter, generate, length, sum, (!), (//))
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)
import Prelude hiding (filter, length, sum)

part1 :: T.Text -> Int
part1 input = solve (parseInstructions input) False

part2 :: T.Text -> Int
part2 input = solve (parseInstructions input) True

data Coord = Coord Int Int deriving (Show, Eq, Ord)

data Block = Block Coord Coord deriving (Show, Eq, Ord)

data Operation = On | Off deriving (Show, Eq, Ord)

data Instruction = Turn Operation Block | Toggle Block deriving (Show, Eq, Ord)

parseInstructions :: T.Text -> [Instruction]
parseInstructions input = parseAoCInput input instructionsParser "instructionsParser"
  where
    numParser = read <$> P.many1 P.digit
    coordParser = Coord <$> (numParser <* P.char ',') <*> numParser
    blockParser = Block <$> (coordParser <* P.string " through ") <*> coordParser
    operationParser = P.choice $ P.try <$> [P.string "on" $> On, P.string "off" $> Off]
    turnParser = Turn <$> (P.string "turn " *> operationParser <* P.space) <*> blockParser
    toggleParser = Toggle <$> (P.string "toggle " *> blockParser)
    instructionParser = P.choice $ P.try <$> [turnParser, toggleParser]
    instructionsParser = P.many1 $ instructionParser <* P.optional P.newline

solve :: [Instruction] -> Bool -> Int
solve instructions brightness =
  if brightness
    then sum finalGrid
    else length $ filter (>= 1) finalGrid
  where
    finalGrid = applyInstructions brightness emptyGrid instructions
    emptyGrid = generate (1000 * 1000) $ const 0

applyInstructions :: Bool -> Vector Int -> [Instruction] -> Vector Int
applyInstructions _ grid [] = grid
applyInstructions brightness grid (instruction : instructions) =
  applyInstructions brightness (updateGrid (getBounds instruction) fn) instructions
  where
    fn val = case instruction of
      Turn On _ -> if brightness then val + 1 else 1
      Turn Off _ -> if brightness then max 0 (val - 1) else 0
      Toggle _ -> if brightness then val + 2 else toggle val

    getBounds (Turn On blk) = blk
    getBounds (Turn Off blk) = blk
    getBounds (Toggle blk) = blk

    toggle 1 = 0
    toggle 0 = 1
    toggle val = val

    updateGrid (Block (Coord blx bly) (Coord trx try)) f =
      grid // [(x * 1000 + y, f $ grid ! (x * 1000 + y)) | x <- [blx .. trx], y <- [bly .. try]]
