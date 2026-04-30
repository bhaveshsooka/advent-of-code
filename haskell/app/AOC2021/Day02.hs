module AOC2021.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = hPos * depth
  where
    (hPos, depth) = foldr move (0, 0) $ parseCommands input
    move (Up val) (x, y) = (x, y - val)
    move (Down val) (x, y) = (x, y + val)
    move (Forward val) (x, y) = (x + val, y)

part2 :: T.Text -> Int
part2 input = hPos * depth
  where
    (hPos, depth, _) = foldl move (0, 0, 0) $ parseCommands input
    move (x, y, a) (Up val) = (x, y, a - val)
    move (x, y, a) (Down val) = (x, y, a + val)
    move (x, y, a) (Forward val) = (x + val, y + (a * val), a)

data Command
  = Up Int
  | Down Int
  | Forward Int
  deriving (Show)

parseCommands :: T.Text -> [Command]
parseCommands input = parseAoCInput input commandsParser "commandParser"
  where
    numParser = read <$> P.many1 P.digit
    upParser = Up <$> (P.string "up " *> numParser)
    downParser = Down <$> (P.string "down " *> numParser)
    forwardParser = Forward <$> (P.string "forward " *> numParser)
    commandParser = P.choice $ P.try <$> [upParser, downParser, forwardParser]
    commandsParser = P.many1 $ commandParser <* P.optional P.newline
