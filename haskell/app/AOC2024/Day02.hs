module AOC2024.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length . filter isReportSafe $ parseReports input

part2 :: T.Text -> Int
part2 input = length . filter isAnyReportVarientSafe $ parseReports input
  where
    isAnyReportVarientSafe xs = any isReportSafe [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

isReportSafe :: [Int] -> Bool
isReportSafe arr = isMonotonic arr && adjLvlDiff arr
  where
    isMonotonic (x : y : xs) = all ((== compare x y) . uncurry compare) (zip (y : xs) xs)
    isMonotonic [_] = True
    isMonotonic [] = True

    adjLvlDiff xs@(_ : rest) = all withinRange (zip xs rest)
    adjLvlDiff [] = True

    withinRange (a, b) = let d = abs (a - b) in d >= 1 && d <= 3

parseReports :: T.Text -> [[Int]]
parseReports input = [[read $ T.unpack x | x <- T.split (== ' ') reportStr] | reportStr <- T.lines input]
