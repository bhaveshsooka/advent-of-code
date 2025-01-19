module AOC2024.Day14
  ( part1,
    part2,
  )
where

import Data.HashMap.Strict qualified as M
import Data.Hashable (Hashable (hashWithSalt))
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Coord (Coord))
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = M.foldrWithKey (\q v acc -> if q /= Mid then acc * length v else acc) 1 quadrantsMap
  where
    quadrantsMap = foldr (quadrantsMapFold . getQuadrant) M.empty newPositions
    quadrantsMapFold (p, q) = M.insertWith (++) q [p]
    getQuadrant c@(Coord x y)
      | x < (wide - 1) `div` 2 && y < (tall - 1) `div` 2 = (c, Q1)
      | x < (wide - 1) `div` 2 && y > (tall - 1) `div` 2 = (c, Q3)
      | x > (wide - 1) `div` 2 && y < (tall - 1) `div` 2 = (c, Q2)
      | x > (wide - 1) `div` 2 && y > (tall - 1) `div` 2 = (c, Q4)
      | otherwise = (c, Mid)
    newPositions = foldr newPositionsFold [] robotStats
    newPositionsFold (p, v) acc = fst (getPosAfterX seconds wide tall (p, v)) : acc
    robotStats = parseRobotStats input
    (seconds, wide, tall) = (100, 101, 103)

part2 :: T.Text -> String
part2 input = visualGrid <> show seconds
  where
    visualGrid = printPositionsInGrid "" (0, 0) (fst <$> finalRobotStats)
    (finalRobotStats, seconds) = findSecondsForChristmasTree (parseRobotStats input) 0
    (wide, tall) = (101, 103)

    findSecondsForChristmasTree robotStats s =
      if nonUniqueLocsCount == 0
        then (newRobotStats, s)
        else findSecondsForChristmasTree robotStats (s + 1)
      where
        nonUniqueLocsCount = M.foldr nonUniqueLocsCountFold (0 :: Int) robotLocsCount
        nonUniqueLocsCountFold v acc = if v > 1 then acc + 1 else acc
        robotLocsCount = foldr robotLocsCountFold M.empty newRobotStats
        robotLocsCountFold (p, _) acc = M.insertWith (if p `M.member` acc then (+) else const id) p (1 :: Int) acc
        newRobotStats = getPosAfterX s wide tall <$> robotStats

    printPositionsInGrid acc (col, row) positions
      | row >= tall = acc
      | otherwise = printPositionsInGrid nextAcc (nextCol, nextRow) positions
      where
        char = if Coord col row `elem` positions then '#' else '.'
        nextAcc = acc ++ [char] ++ (if col == wide - 1 then "\n" else "")
        nextCol = if col == wide - 1 then 0 else col + 1
        nextRow = if col == wide - 1 then row + 1 else row

type Velocity = (Int, Int)

type RobotStat = (Coord, Velocity)

type RobotStats = [RobotStat]

data Quadrant = Q1 | Q2 | Q3 | Q4 | Mid deriving (Eq, Show, Ord)

instance Hashable Quadrant where
  hashWithSalt :: Int -> Quadrant -> Int
  hashWithSalt _ Q1 = 1
  hashWithSalt _ Q2 = 2
  hashWithSalt _ Q3 = 3
  hashWithSalt _ Q4 = 4
  hashWithSalt _ Mid = 5

getPosAfterX :: Int -> Int -> Int -> (Coord, Velocity) -> (Coord, Velocity)
getPosAfterX seconds wide tall (Coord x y, (vx, vy)) = (newPos, (vx, vy))
  where
    newPos = Coord (newX `mod` wide) (newY `mod` tall)
    newX = x + (vx * seconds)
    newY = y + (vy * seconds)

parseRobotStats :: T.Text -> RobotStats
parseRobotStats input = parseAoCInput input robotStatsParser "robotStatsParser"
  where
    numParser =
      P.choice
        [ read <$> P.many1 P.digit,
          negate . read <$> (P.char '-' *> P.many1 P.digit)
        ]
    positionParser = Coord <$> (P.string "p=" *> numParser) <*> (P.char ',' *> numParser)
    velocityParser = (,) <$> (P.string " v=" *> numParser) <*> (P.char ',' *> numParser)
    robotStatParser = (,) <$> positionParser <*> velocityParser
    robotStatsParser = P.many1 $ robotStatParser <* P.optional P.newline
