module AOC2017.Day02
  ( part1,
    part2,
  )
where

import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ListUtils (combinations)
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\r acc -> acc + (maximum r - minimum r)) 0 $ parseSpreadsheet input

part2 :: T.Text -> Int
part2 input = foldr (\ps acc -> acc + calcQuotient ps) 0 twoCombos
  where
    calcQuotient = foldr (\p acc -> acc + getQuotient p) 0
    getQuotient [a, b] = if b `mod` a == 0 then b `div` a else 0
    getQuotient _ = 0
    twoCombos = (sort <$>) . combinations 2 <$> sheet
    sheet = parseSpreadsheet input

parseSpreadsheet :: T.Text -> [[Int]]
parseSpreadsheet input = parseAoCInput input spreadsheetParser "spreadsheetParser"
  where
    numParser = read <$> P.many1 P.digit
    hspace = P.skipMany1 (P.choice [P.char ' ', P.char '\t'])
    rowParser = P.sepBy1 numParser hspace
    spreadsheetParser = P.many1 $ rowParser <* P.optional P.newline
