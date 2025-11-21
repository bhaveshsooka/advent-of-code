module AOC2018.Day04
  ( part1,
    part2,
  )
where

import Data.List (sort)
import Data.Map qualified as M
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = gid' * minute'
  where
    (gid', minute', _) =
      M.foldrWithKey
        (\k (m, _, v) (gid, minute, currMax) -> if v > currMax then (k, m, v) else (gid, minute, currMax))
        (0, 0, 0)
        sleepStats
    sleepStats = sleepySleepySleepy <$> guardsSleepCounts
    sleepySleepySleepy =
      M.foldrWithKey
        (\k v (maxK, maxV, s) -> if v >= maxV then (k, v, s + v) else (maxK, maxV, s + v))
        (0, 0, 0)
    guardsSleepCounts = calcSleepCounts M.empty <$> windowedRecords
    windowedRecords = (\xs -> [(x, y) | (x, y) <- zip xs (drop 1 xs)]) <$> recordsPerGuard
    recordsPerGuard = groupRecords records (0, []) M.empty
    records = sort $ parseRecords input

part2 :: T.Text -> Int
part2 input = gid' * minute'
  where
    (gid', minute', _) =
      M.foldrWithKey
        (\k (m, c, _) (gid, minute, currMax) -> if c > currMax then (k, m, c) else (gid, minute, currMax))
        (0, 0, 0)
        sleepStats
    sleepStats = sleepySleepySleepy <$> guardsSleepCounts
    sleepySleepySleepy =
      M.foldrWithKey
        (\k v (maxK, maxV, s) -> if v >= maxV then (k, v, s + v) else (maxK, maxV, s + v))
        (0, 0, 0)
    guardsSleepCounts = calcSleepCounts M.empty <$> windowedRecords
    windowedRecords = (\xs -> [(x, y) | (x, y) <- zip xs (drop 1 xs)]) <$> recordsPerGuard
    recordsPerGuard = groupRecords records (0, []) M.empty
    records = sort $ parseRecords input

type ID = Int

data Timestamp = Timestamp Int Int Int Int Int deriving (Show)

data Event = Sleep | Wake | ShiftStart ID deriving (Show)

data Record = Record Timestamp Event deriving (Show)

type SleepCounts = M.Map Int Int

calcSleepCounts :: SleepCounts -> [(Record, Record)] -> SleepCounts
calcSleepCounts counts [] = counts
calcSleepCounts counts ((r1, r2) : xs) =
  case (e2, d1 == d2) of
    (Sleep, True) -> calcSleepCounts (foldr (\m acc -> M.insertWith (+) m 1 acc) counts [m2' .. m1' - 1]) xs
    _ -> calcSleepCounts counts xs
  where
    (Record (Timestamp _ _ d1 _ m1') _) = r1
    (Record (Timestamp _ _ d2 _ m2') e2) = r2

groupRecords :: [Record] -> (ID, [Record]) -> M.Map ID [Record] -> M.Map ID [Record]
groupRecords [] (k, v) acc = if k /= 0 then M.insertWith (++) k v acc else acc
groupRecords (x@(Record _ e) : xs) (k, v) acc =
  case e of
    ShiftStart gid -> groupRecords xs (gid, []) (if k /= 0 then M.insertWith (++) k v acc else acc)
    Sleep -> groupRecords xs (k, x : v) acc
    Wake -> groupRecords xs (k, x : v) acc

parseRecords :: T.Text -> [Record]
parseRecords input = parseAoCInput input recordsParser "recordsParser"
  where
    numParser = read <$> P.many1 P.digit
    timestampParser =
      Timestamp
        <$> (P.char '[' *> numParser)
        <*> (P.char '-' *> numParser)
        <*> (P.char '-' *> numParser)
        <*> (P.space *> numParser)
        <*> (P.char ':' *> numParser <* P.string "] ")
    sleepParser = Sleep <$ P.string "falls asleep"
    wakeParser = Wake <$ P.string "wakes up"
    shiftStartParser = ShiftStart <$> (P.string "Guard #" *> numParser <* P.string " begins shift")
    eventParser = P.choice $ P.try <$> [sleepParser, wakeParser, shiftStartParser]
    recordParser = Record <$> timestampParser <*> eventParser
    recordsParser = P.many1 $ recordParser <* P.optional P.newline

instance Eq Timestamp where
  (==) :: Timestamp -> Timestamp -> Bool
  (Timestamp y1 m1 d1 h1 min1) == (Timestamp y2 m2 d2 h2 min2) =
    (y1, m1, d1, h1, min1) == (y2, m2, d2, h2, min2)

instance Ord Timestamp where
  compare :: Timestamp -> Timestamp -> Ordering
  compare (Timestamp y1 m1 d1 h1 min1) (Timestamp y2 m2 d2 h2 min2) =
    compare (y1, m1, d1, h1, min1) (y2, m2, d2, h2, min2)

instance Eq Record where
  (==) :: Record -> Record -> Bool
  (Record ts1 _) == (Record ts2 _) = ts1 == ts2

instance Ord Record where
  compare :: Record -> Record -> Ordering
  compare (Record ts1 _) (Record ts2 _) = compare ts1 ts2
