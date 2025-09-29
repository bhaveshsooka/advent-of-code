module AOC2015.Day03
  ( part1,
    part2,
  )
where

import Data.Set qualified as S
import Data.Text qualified as T
import Util.GridUtils.Coord (Coord (Coord))

part1 :: T.Text -> Int
part1 input = length . snd $ T.foldl go (mempty, S.singleton mempty) input
  where
    go (acc, v) c = (acc <> getCoord c, S.insert (acc <> getCoord c) v)

part2 :: T.Text -> Int
part2 input = length . third $ T.foldl go (0 :: Int, (mempty, mempty), S.singleton mempty) input
  where
    third (_, _, x) = x
    go (i, (acc1, acc2), v) c =
      if even i
        then (i + 1, (acc1 <> getCoord c, acc2), S.insert (acc1 <> getCoord c) v)
        else (i + 1, (acc1, acc2 <> getCoord c), S.insert (acc2 <> getCoord c) v)

getCoord :: Char -> Coord
getCoord c = case c of
  '^' -> Coord 0 1
  'v' -> Coord 0 (-1)
  '<' -> Coord (-1) 0
  '>' -> Coord 1 0
  _ -> error "Invalid direction"
