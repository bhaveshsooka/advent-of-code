module AOC2024.Day25
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ filter id $ tryAKey <$> keyLockPairs
  where
    tryAKey (key, lock) = and $ zipWith (\x y -> x + y <= 5) key lock
    keyLockPairs = [(x, y) | x <- keys, y <- locks]
    (keys, locks) = parseKeysAndLocks input

part2 :: T.Text -> Int
part2 _input = 0

parseKeysAndLocks :: T.Text -> ([[Int]], [[Int]])
parseKeysAndLocks input = (makeKeyOrLock <$> keyGrids, makeKeyOrLock <$> lockGrids)
  where
    makeKeyOrLock grid = foldl (zipWith (+)) (replicate 5 (-1)) (intLineFold [] <$> grid)
    intLineFold acc gridLine = acc ++ (toNum <$> T.unpack gridLine)
    (keyGrids, lockGrids) = foldr keysAndLocksFold ([], []) grids
    grids = T.splitOn (T.pack "\n") <$> T.splitOn (T.pack "\n\n") input
    keysAndLocksFold x (ks, lks) = if T.unpack (last x) == "#####" then (x : ks, lks) else (ks, x : lks)
    toNum x = (if x == '#' then 1 else 0) :: Int
