module AOC2015.Day11
  ( part1,
    part2,
  )
where

import Data.Char (chr, ord)
import Data.List (group)
import Data.Text qualified as T

part1 :: T.Text -> String
part1 input = nextValidPassword (T.unpack input)

part2 :: T.Text -> String
part2 input = nextValidPassword $ part1 input

increasingStraight :: String -> Bool
increasingStraight [] = False
increasingStraight [_] = False
increasingStraight [_, _] = False
increasingStraight (x : y : z : rest) = consecutive || increasingStraight (y : z : rest)
  where
    consecutive = (ord y - ord x) == 1 && (ord z - ord y) == 1

hasPairs :: String -> Bool
hasPairs str = length (filter ((== 2) . length) $ group str) > 1

hasForbiddenChars :: String -> Bool
hasForbiddenChars str = any (`elem` str) "iol"

nextPassword :: String -> String
nextPassword = reverse . increment . reverse
  where
    increment [] = ['a']
    increment (c : cs)
      | c == 'z' = 'a' : increment cs
      | otherwise = chr (ord c + 1) : cs

nextValidPassword :: String -> String
nextValidPassword password
  | isValid (nextPassword password) = nextPassword password
  | otherwise = nextValidPassword (nextPassword password)
  where
    isValid pwd = increasingStraight pwd && hasPairs pwd && not (hasForbiddenChars pwd)
