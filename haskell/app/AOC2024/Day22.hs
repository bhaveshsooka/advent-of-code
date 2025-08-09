module AOC2024.Day22
  ( part1,
    part2,
  )
where

import Data.Bits (Bits (xor))
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr ((+) . get2000thSecret) 0 $ parseSecretNumbers input
  where
    get2000thSecret s = foldr (const nextSecret) s ([1 .. 2000] :: [Int])

part2 :: T.Text -> Int
part2 _input = 0

data Op = Mix | Prune deriving (Show, Eq, Ord)

type SecretNumbers = V.Vector Int

nextSecret :: Int -> Int
nextSecret secretNumber = mixOrPrune Prune 0 val3
  where
    val = mixOrPrune Mix (secretNumber * 64) secretNumber
    step1Result = mixOrPrune Prune 0 val
    val2 = mixOrPrune Mix (step1Result `div` 32) step1Result
    step2Result = mixOrPrune Prune 0 val2
    val3 = mixOrPrune Mix (step2Result * 2048) step2Result
    mixOrPrune op v sn = case op of Mix -> v `xor` sn; Prune -> sn `mod` 16777216

parseSecretNumbers :: T.Text -> SecretNumbers
parseSecretNumbers input = parseAoCInput input secretNumbersParser "secretNumbersParser"
  where
    secretNumberParser = read <$> P.many1 P.digit
    secretNumbersParser = V.fromList <$> P.many1 (secretNumberParser <* P.optional P.newline)
