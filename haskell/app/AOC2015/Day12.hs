module AOC2015.Day12
  ( part1,
    part2,
  )
where

import Data.Aeson qualified as AE
import Data.Scientific qualified as S
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ parseNumbers input

part2 :: T.Text -> Int
part2 input = case AE.decodeStrictText @AE.Value input of
  Just json -> case S.toBoundedInteger $ sum $ parseNonRedNumbers json of
    Just total -> total
    _ -> error "Failed to convert sum to Int"
  Nothing -> error "Failed to parse JSON input"

parseNonRedNumbers :: AE.Value -> [S.Scientific]
parseNonRedNumbers (AE.Object obj)
  | AE.String (T.pack "red") `elem` obj = []
  | otherwise = foldl (\acc v -> parseNonRedNumbers v ++ acc) [] obj
parseNonRedNumbers (AE.Array arr) = foldl (\acc v -> parseNonRedNumbers v ++ acc) [] arr
parseNonRedNumbers (AE.Number n) = pure n
parseNonRedNumbers _ = []

parseNumbers :: T.Text -> [Int]
parseNumbers input = parseAoCInput input extractNumsParser "extractNumsParser"
  where
    numParser =
      P.choice
        [ read <$> P.many1 P.digit,
          negate . read <$> (P.char '-' *> P.many1 P.digit)
        ]
    numsParser = P.choice [P.try numParser, P.try $ (0 :: Int) <$ P.anyChar]
    extractNumsParser = P.many1 $ numsParser <* P.optional P.newline
