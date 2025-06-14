module AOC2024.Day24
  ( part1,
    part2,
  )
where

import Data.Bits (xor, (.&.), (.|.))
import Data.HashMap.Strict qualified as M
import Data.List (isPrefixOf)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = M.foldrWithKey zGatetoDec 0 $ processGates memo gates
  where
    zGatetoDec k v acc = if "z" `isPrefixOf` k && v then acc + (2 ^ (read (drop 1 k) :: Int)) else acc
    (memo, gates) = parseWiresAndGates input

part2 :: T.Text -> Int
part2 _input = 0

type Label = String

type Memo = M.HashMap Label Bool

newtype Input = Input Label deriving (Show)

newtype Output = Output Label deriving (Show)

data Gate
  = AND Input Input Output
  | OR Input Input Output
  | XOR Input Input Output
  deriving (Show)

type Gates = [Gate]

processGates :: Memo -> Gates -> Memo
processGates memo [] = memo
processGates memo (g : gs) =
  if M.member label1 memo && M.member label2 memo
    then processGates (processGate g) gs
    else processGates memo $ gs ++ [g]
  where
    (label1, label2) = case g of
      AND (Input w1) (Input w2) _ -> (w1, w2)
      OR (Input w1) (Input w2) _ -> (w1, w2)
      XOR (Input w1) (Input w2) _ -> (w1, w2)
    processGate (AND (Input w1) (Input w2) (Output w3)) = M.insert w3 (memo M.! w1 .&. memo M.! w2) memo
    processGate (OR (Input w1) (Input w2) (Output w3)) = M.insert w3 (memo M.! w1 .|. memo M.! w2) memo
    processGate (XOR (Input w1) (Input w2) (Output w3)) = M.insert w3 (memo M.! w1 `xor` memo M.! w2) memo

parseWiresAndGates :: T.Text -> (Memo, Gates)
parseWiresAndGates input = parseAoCInput input wiresAndGatesParser "wiresAndGatesParser"
  where
    boolParser = P.choice [True <$ P.string "1", False <$ P.string "0"]
    wireParser = (,) <$> (P.many1 P.alphaNum <* P.string ": ") <*> boolParser
    inputParser tag = Input <$> P.many1 P.alphaNum <* P.string tag
    outputParser = Output <$> P.many1 P.alphaNum
    andParser = AND <$> inputParser " AND " <*> inputParser " -> " <*> outputParser
    orParser = OR <$> inputParser " OR " <*> inputParser " -> " <*> outputParser
    xorParser = XOR <$> inputParser " XOR " <*> inputParser " -> " <*> outputParser
    gateParser = P.choice $ P.try <$> [andParser, orParser, xorParser]
    gatesParser = P.many1 $ gateParser <* P.optional P.newline
    wiresParser = P.many1 $ wireParser <* P.optional P.newline
    memoParser = M.fromList <$> wiresParser
    wiresAndGatesParser = (,) <$> (memoParser <* P.optional P.newline) <*> gatesParser
