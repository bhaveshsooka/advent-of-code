module AOC2016.Day02
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Text qualified as T

part1 :: T.Text -> String
part1 input = concat $ mapMaybe (`M.lookup` keypad) passwordCoords
  where
    passwordCoords = drop 1 $ scanl (processInstruction False) start instructions
    instructions = T.unpack <$> T.lines input
    start = Coord 1 1

part2 :: T.Text -> String
part2 input = concat $ mapMaybe (`M.lookup` fancyKeypad) passwordCoords
  where
    passwordCoords = drop 1 $ scanl (processInstruction True) start instructions
    instructions = T.unpack <$> T.lines input
    start = Coord 2 0

data Coord = Coord Int Int deriving (Show, Eq, Ord)

data Direction = U | D | L | R deriving (Show, Eq)

processInstruction :: Bool -> Coord -> String -> Coord
processInstruction _ coord [] = coord
processInstruction isPart2 coord (i : is) =
  if isValid newCoord
    then processInstruction isPart2 newCoord is
    else processInstruction isPart2 coord is
  where
    newCoord = move coord $ parseDir i
    isValid (Coord x y) =
      if isPart2
        then M.member (Coord x y) fancyKeypad
        else M.member (Coord x y) keypad

fancyKeypad :: M.Map Coord String
fancyKeypad =
  M.fromList
    [ (Coord 0 2, "1"),
      (Coord 1 1, "2"),
      (Coord 1 2, "3"),
      (Coord 1 3, "4"),
      (Coord 2 0, "5"),
      (Coord 2 1, "6"),
      (Coord 2 2, "7"),
      (Coord 2 3, "8"),
      (Coord 2 4, "9"),
      (Coord 3 1, "A"),
      (Coord 3 2, "B"),
      (Coord 3 3, "C"),
      (Coord 4 2, "D")
    ]

keypad :: M.Map Coord String
keypad = M.fromList $ [(Coord x y, c) | x <- [0 .. 2], y <- [0 .. 2], let c = show $ x * 3 + y + 1]

parseDir :: Char -> Direction
parseDir 'U' = U
parseDir 'D' = D
parseDir 'L' = L
parseDir 'R' = R
parseDir _ = error "invalid direction"

move :: Coord -> Direction -> Coord
move (Coord x y) U = Coord (x - 1) y
move (Coord x y) D = Coord (x + 1) y
move (Coord x y) L = Coord x (y - 1)
move (Coord x y) R = Coord x (y + 1)
