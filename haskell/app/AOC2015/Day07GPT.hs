{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day07GPT (
  parts,
) where

import Data.Bits (complement, shift, shiftR, (.&.), (.|.))
import Data.Char (isDigit)
import Data.Map (Map, fromList, insert, keys, lookup, member)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Word (Word16)
import Util.AOCHelpers (Part (Part), Parts)
import Prelude hiding (lookup)

parts :: Parts
parts = (Part part1, Part part2)

part1 :: T.Text -> Word16
part1 input = fromJust $ lookup "a" $ processSignals (signals input) mempty

part2 :: T.Text -> Word16
part2 input = fromJust $ lookup "a" $ processSignals (signals input) (insert "b" (part1 input) mempty)

signals :: T.Text -> Map T.Text Op
signals input = fromList $ parseLine <$> T.lines input

data Wire = WInt Word16 | WStr T.Text deriving (Show, Eq)

data Op
  = AND Wire Wire
  | OR Wire Wire
  | LSHIFT Wire Int
  | RSHIFT Wire Int
  | NOT Wire
  | DIRECT Wire
  deriving (Show, Eq)

parseLine :: T.Text -> (T.Text, Op)
parseLine s =
  case T.words s of
    [a, "->", b] -> (b, DIRECT (parseWire a))
    [a, "AND", b, "->", c] -> (c, AND (parseWire a) (parseWire b))
    [a, "OR", b, "->", c] -> (c, OR (parseWire a) (parseWire b))
    [a, "LSHIFT", b, "->", c] -> (c, LSHIFT (parseWire a) (read . T.unpack $ b))
    [a, "RSHIFT", b, "->", c] -> (c, RSHIFT (parseWire a) (read . T.unpack $ b))
    ["NOT", a, "->", b] -> (b, NOT (parseWire a))
    _ -> error ("Can't parse: " ++ show s)

parseWire :: T.Text -> Wire
parseWire s =
  if all isDigit $ T.unpack s
    then WInt (read $ T.unpack s)
    else WStr s

-- Memoization cache for computed values
type Cache = Map T.Text Word16

processSignals :: Map T.Text Op -> Cache -> Cache
processSignals m cache = foldl (processSignal m) cache (keys m)

processSignal :: Map T.Text Op -> Cache -> T.Text -> Cache
processSignal m cache key
  | key `member` cache = cache
  | otherwise =
      let val = case fromJust $ lookup key m of
            DIRECT w -> evalWire m cache w
            AND w1 w2 -> evalWire m cache w1 .&. evalWire m cache w2
            OR w1 w2 -> evalWire m cache w1 .|. evalWire m cache w2
            LSHIFT w i -> evalWire m cache w `shift` i
            RSHIFT w i -> evalWire m cache w `shiftR` i
            NOT w -> complement (evalWire m cache w)
       in insert key val cache

evalWire :: Map T.Text Op -> Cache -> Wire -> Word16
evalWire _ _ (WInt i) = i
evalWire m cache (WStr s) = fromJust $ lookup s (processSignals m cache)
