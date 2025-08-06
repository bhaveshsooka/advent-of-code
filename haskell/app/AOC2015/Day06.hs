module AOC2015.Day06
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector.Unboxed.Mutable qualified as MV
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Block, Coord (..))
import Util.ParseHelpers (parseAoCInput)
import Control.Monad (forM_)
import GHC.IO (unsafePerformIO)
import qualified Data.Vector.Unboxed as V

part1 :: T.Text -> Int
part1 input = unsafePerformIO $ do
  grid <- MV.replicate (1000 * 1000) 0 :: IO (MV.IOVector Int)
  mapM_ (processInstruction False grid 1000) (parseInstructions input)
  V.sum <$> V.freeze grid

part2 :: T.Text -> Int
part2 input = unsafePerformIO $ do
  grid <- MV.replicate (1000 * 1000) 0 :: IO (MV.IOVector Int)
  mapM_ (processInstruction True grid 1000) (parseInstructions input)
  V.sum <$> V.freeze grid

data Instruction = TurnOn Block | TurnOff Block | Toggle Block deriving (Show, Eq)

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

processInstruction :: Bool -> MV.IOVector Int -> Int -> Instruction -> IO ()
processInstruction b grid numCols i = do
  case (i, b) of
    (TurnOn block, False) -> updateGrid block (const 1)
    (TurnOff block, False) -> updateGrid block (const 0)
    (Toggle block, False) -> updateGrid block (\v -> if v == 0 then 1 else 0)
    (TurnOn block, True) -> updateGrid block (+ 1)
    (TurnOff block, True) -> updateGrid block (\v -> if v == 0 then 0 else v - 1)
    (Toggle block, True) -> updateGrid block (+ 2)
  where
    updateGrid (Coord x1 y1, Coord x2 y2) fn =
      forM_ [y1 .. y2] $ \y ->
        forM_ [x1 .. x2] $ \x -> do
          let idx = x * numCols + y
          val <- MV.read grid idx
          MV.write grid idx (fn val)
