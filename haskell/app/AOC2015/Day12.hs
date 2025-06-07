module AOC2015.Day12
  ( part1,
    part2,
  )
where

import Data.Aeson qualified as AE
import Data.Scientific qualified as S
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = truncate . sum $ parseNumbers (parseJson input) False

part2 :: T.Text -> Int
part2 input = truncate . sum $ parseNumbers (parseJson input) True

parseNumbers :: AE.Value -> Bool -> [S.Scientific]
parseNumbers (AE.Object obj) excludeRed
  | excludeRed && AE.String (T.pack "red") `elem` obj = []
  | otherwise = foldl (\acc v -> parseNumbers v excludeRed ++ acc) [] obj
parseNumbers (AE.Array arr) excludeRed = foldl (\acc v -> parseNumbers v excludeRed ++ acc) [] arr
parseNumbers (AE.Number n) _ = pure n
parseNumbers _ _ = []

parseJson :: T.Text -> AE.Value
parseJson input = case AE.decodeStrictText @AE.Value input of
  Just json -> json
  Nothing -> error "Failed to parse JSON input"
