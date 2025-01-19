module AOC2024.Day13
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ tokensSpent <$> intSolutions
  where
    tokensSpent x = foldr (\(a, b) acc -> acc + (a * b)) 0 (zip [3, 1] x)
    intSolutions = (\x -> round . fst <$> x) <$> filteredDiffs
    eps = 1.0e-3 -- 0.001 a small enough number to detect rounding errors (hopefully)
    filteredDiffs = filter filterFn diffsForRows
    filterFn = all (\(_, diff) -> diff < eps)
    diffsForRows = diffsForRow <$> optimalMoves
    diffsForRow (x :: [Double]) = (\e -> (e, abs (fromIntegral (round e :: Int) - e))) <$> x
    optimalMoves = solve . clawMachineToMatrix factor <$> parseClawMachines input
    factor = 0

part2 :: T.Text -> Int
part2 input = sum $ tokensSpent <$> intSolutions
  where
    tokensSpent x = foldr (\(a, b) acc -> acc + (a * b)) 0 (zip [3, 1] x)
    intSolutions = (\x -> round . fst <$> x) <$> filteredDiffs
    eps = 1.0e-3 -- 0.001 a small enough number to detect rounding errors (hopefully)
    filteredDiffs = filter filterFn diffsForRows
    filterFn = all (\(_, diff) -> diff < eps)
    diffsForRows = diffsForRow <$> optimalMoves
    diffsForRow (x :: [Double]) = (\e -> (e, abs (fromIntegral (round e :: Int) - e))) <$> x
    optimalMoves = solve . clawMachineToMatrix factor <$> parseClawMachines input
    factor = 10000000000000

type Row = [Double]

type Matrix = [Row]

type Button = (Int, Int)

type Prize = (Int, Int)

data ClawMachine = ClawMachine Button Button Prize deriving (Show)

clawMachineToMatrix :: Int -> ClawMachine -> Matrix
clawMachineToMatrix factor (ClawMachine (x1, y1) (x2, y2) (x3, y3)) =
  [ [fromIntegral x1, fromIntegral x2, fromIntegral (factor + x3)],
    [fromIntegral y1, fromIntegral y2, fromIntegral (factor + y3)]
  ]

parseClawMachines :: T.Text -> [ClawMachine]
parseClawMachines input = parseAoCInput input clawMachinesParser "clawMachinesParser"
  where
    numParserVoidLStr voidStr = P.string voidStr *> (read <$> P.many1 P.digit)
    buttonParser lab = (,) <$> numParserVoidLStr ("Button " <> lab <> ": X+") <*> numParserVoidLStr ", Y+" <* P.newline
    prizeParser = (,) <$> numParserVoidLStr "Prize: X=" <*> numParserVoidLStr ", Y=" <* P.optional P.newline
    clawMachineParser = ClawMachine <$> buttonParser "A" <*> buttonParser "B" <*> prizeParser
    clawMachinesParser = P.many1 $ clawMachineParser <* P.optional P.newline

solve :: Matrix -> Row
solve = substitute . gaussianReduce

-- Solve a matrix (must already be in REF form) by back substitution.
substitute :: Matrix -> Row
substitute matrix = foldr next [last (last matrix)] (init matrix)
  where
    next row found = solution : found
      where
        subpart = init $ drop (length matrix - length found) row
        solution = last row - sum (zipWith (*) found subpart)

gaussianReduce :: Matrix -> Matrix
gaussianReduce matrix = fixlastrow $ foldr reduceRow matrix [0 .. length matrix - 1]
  where
    -- swaps element at position a with element at position b.
    swap xs a b
      | a > b = swap xs b a
      | a == b = xs
      | a < b = p1 ++ [xs !! b] ++ p3 ++ [xs !! a] ++ p4
      | otherwise = error "list is too short"
      where
        (p1, p2) = case splitAt a xs of
          (p1', _ : p2') -> (p1', p2')
          _ -> error "list is too short"
        (p3, p4) = case splitAt (b - a - 1) p2 of
          (p3', _ : p4') -> (p3', p4')
          _ -> error "list is too short"

    -- concat the lists and repeat
    reduceRow r matrix1 = take r matrix2 ++ [row1] ++ nextrows
      where
        -- first non-zero element on or below (r,r).
        filteredMatrixRow = filter (\x -> matrix1 !! x !! r /= 0) [r .. length matrix1 - 1]
        firstnonzero = case filteredMatrixRow of
          [] -> error "matrix is singular"
          (x : _) -> x

        -- matrix with row swapped (if needed)
        matrix2 = swap matrix1 r firstnonzero

        -- row we're working with
        row = matrix2 !! r

        -- make it have 1 as the leading coefficient
        row1 = map (\x -> x / (row !! r)) row

        -- subtract nr from row1 while multiplying
        subrow nr = let k = nr !! r in zipWith (\a b -> k * a - b) row1 nr

        -- apply subrow to all rows below
        nextrows = map subrow $ drop (r + 1) matrix2

    fixlastrow matrix' = a ++ [init (init row) ++ [1, z / nz]]
      where
        a = init matrix'
        row = last matrix'
        z = last row
        nz = last (init row)
