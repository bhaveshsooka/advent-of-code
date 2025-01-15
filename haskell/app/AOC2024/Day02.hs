module AOC2024.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ filter isArrOrReversedSafe $ parseReports input
  where
    isArrOrReversedSafe arr = isReportSafe arr || isReportSafe (reverse arr)

part2 :: T.Text -> Int
part2 input = length . filter isAnyReportVarientSafe $ parseReports input
  where
    isAnyReportVarientSafe arr = any isArrOrReversedSafe (allReportVarients arr)
    isArrOrReversedSafe arr = isReportSafe arr || isReportSafe (reverse arr)
    allReportVarients arr = foldr (varientsFold arr) [] [0 .. length arr - 1]
    varientsFold arr i acc = snd (foldr (deleteIFold i) (0, []) arr) : acc
    deleteIFold i e (idx, acc) = if i == idx then (idx + 1, acc) else (idx + 1, acc ++ [e])

isReportSafe :: [Int] -> Bool
isReportSafe arr = foldr (\(x, y) acc -> acc && isValidPair (x, y)) True $ pairs arr
  where
    pairs [] = []
    pairs [_] = []
    pairs (x : y : xs) = (x, y) : pairs (y : xs)
    isValidPair (x, y) = x <= y && (abs (x - y) <= 3) && x /= y

parseReports :: T.Text -> [[Int]]
parseReports input = reportList <$> T.lines input
  where
    reportList reportLine = read . T.unpack <$> T.splitOn (T.pack " ") reportLine
