module AOC2015.Day06 (
  parts,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST qualified as STA
import Data.Functor (($>))
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.AOCHelpers (Part (Part), Parts, parseAoCInput)

parts :: Parts
parts = (Part part1, Part part2)

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
solve instructions brightness = runST $ do
  grid <- emptyArray 1000 1000
  forM_ instructions (applyInstruction grid brightness)
  elements <- STA.getAssocs grid
  if brightness
    then return $ sum $ map snd elements
    else return $ length $ filter (>= 1) $ map snd elements
 where
  emptyArray rows cols = STA.newArray ((0, 0), (rows - 1, cols - 1)) 0

applyInstruction :: STA.STArray s (Int, Int) Int -> Bool -> Instruction -> ST s ()
applyInstruction grid brightness instruction = do
  let (Coord blx bly, Coord trx try_) = getBounds instruction
  forM_ [blx .. trx] $ \x ->
    forM_ [bly .. try_] $ \y -> do
      val <- STA.readArray grid (x, y)
      STA.writeArray grid (x, y) $ updateValue val instruction
 where
  getBounds (Turn On (Block bl tr)) = (bl, tr)
  getBounds (Turn Off (Block bl tr)) = (bl, tr)
  getBounds (Toggle (Block bl tr)) = (bl, tr)

  updateValue val (Turn On _) = if brightness then val + 1 else 1
  updateValue val (Turn Off _) = if brightness then max 0 (val - 1) else 0
  updateValue val (Toggle _) = if brightness then val + 2 else toggle val

  toggle 1 = 0
  toggle 0 = 1
  toggle x = x
