module AOC2016.Day02
  ( part1,
    part2,
  )
where

import Data.Char (intToDigit)
import Data.Map qualified as M
import Data.Text qualified as T
import Util.GridUtils.Coord (Coord (Coord))

part1 :: T.Text -> String
part1 input = foldr (\c acc -> keypad M.! c : acc) "" passwordCoords
  where
    passwordCoords = drop 1 $ scanl (processInstruction False) start instructions
    instructions = T.unpack <$> T.lines input
    start = Coord 1 1

part2 :: T.Text -> String
part2 input = foldr (\c acc -> fancyKeypad M.! c : acc) "" passwordCoords
  where
    passwordCoords = drop 1 $ scanl (processInstruction True) start instructions
    instructions = T.unpack <$> T.lines input
    start = Coord 2 0

processInstruction :: Bool -> Coord -> String -> Coord
processInstruction isPart2 = foldl moveOnKeypad
  where
    moveOnKeypad acc i
      | isPart2 = if M.member (move acc i) fancyKeypad then move acc i else acc
      | M.member (move acc i) keypad = move acc i
      | otherwise = acc

    move (Coord x y) 'U' = Coord (x - 1) y
    move (Coord x y) 'D' = Coord (x + 1) y
    move (Coord x y) 'L' = Coord x (y - 1)
    move (Coord x y) 'R' = Coord x (y + 1)
    move coord _ = coord

fancyKeypad :: M.Map Coord Char
fancyKeypad = M.fromList $ row0 ++ row1 ++ row2 ++ row3
  where
    row0 = [(Coord 0 2, '1')]
    row1 = [(Coord 1 y, intToDigit (y + 1)) | y <- [1 .. 3]]
    row2 = [(Coord 2 y, intToDigit (y + 5)) | y <- [0 .. 4]]
    row3 = [(Coord 3 1, 'A'), (Coord 3 2, 'B'), (Coord 3 3, 'C'), (Coord 4 2, 'D')]

keypad :: M.Map Coord Char
keypad = M.fromList $ [(Coord x y, c) | x <- [0 .. 2], y <- [0 .. 2], let c = intToDigit $ x * 3 + y + 1]
