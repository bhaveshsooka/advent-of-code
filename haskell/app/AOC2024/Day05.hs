module AOC2024.Day05
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T

-- part1 :: T.Text -> Int
part1 input = parseInput input
  where
    (rules, updates) = parseInput input

part2 :: T.Text -> Int
part2 input = 0

-- checkUpdate :: [M.Map Int Int] -> M.Map Int Int -> Bool
-- checkUpdate rules update =

parseInput :: T.Text -> ([M.Map Int Int], [M.Map Int Int])
parseInput input = (rules, updates)
  where
    rules = parseBySep (T.pack "|") <$> T.lines (head inputSets)
    updates = parseBySep (T.pack ",") <$> T.lines (last inputSets)
    inputSets = T.splitOn (T.pack "\n\n") input
    parseBySep sep line = M.fromList $ zip (read . T.unpack <$> T.splitOn sep line) [0 ..]
