module AOC2016.Day07
  ( part1,
    part2,
  )
where

import Control.Monad (guard)
import Data.List (isInfixOf, tails)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length . findAbbAs $ parseBlocks input
  where
    findAbbAs = filter $ \blocks -> not (any isABBA ([s | H s <- blocks])) && any isABBA [s | S s <- blocks]

part2 :: T.Text -> Int
part2 input = length . findSSLs $ parseBlocks input
  where
    findSSLs = filter $ \blocks -> do
      let hypers = [s | H s <- blocks]
      or
        [ any (isInfixOf [b, a, b]) hypers
          | S s <- blocks,
            [a, b, _] <- abasOf s
        ]

data Block a
  = S a
  | H a
  deriving (Show, Eq, Functor)

isABBA :: String -> Bool
isABBA = any ((\x -> length x == 4 && good x) . take 4) . tails
  where
    good [a, b, c, d] = a == d && b == c && b /= a
    good _ = False

abasOf :: String -> [String]
abasOf = concatMap ((\x -> guard (length x == 3) >> good x) . take 3) . tails
  where
    good k@[a, b, c] = [k | a == c && a /= b]
    good _ = []

parseBlocks :: T.Text -> [[Block String]]
parseBlocks input = parseBlockList [] "" . T.unpack <$> T.lines input
  where
    parseBlockList :: [Block String] -> String -> String -> [Block String]
    parseBlockList blocks accumStr [] = blocks ++ [S accumStr]
    parseBlockList blocks accumStr (c : rest) =
      case c of
        '[' -> parseBlockList (blocks ++ [S accumStr]) "" rest
        ']' -> parseBlockList (blocks ++ [H accumStr]) "" rest
        _ -> parseBlockList blocks (accumStr ++ [c]) rest
