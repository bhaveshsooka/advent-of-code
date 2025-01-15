module AOC2015.Day07
  ( part1,
    part2,
  )
where

import Data.Bits (Bits (complement, shift, shiftR, (.&.), (.|.)))
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Word (Word16)
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Word16
part1 input = processSignals (parseSignals input) M.empty M.! "a"

part2 :: T.Text -> Word16
part2 input = processSignals (parseSignals input) initSignals M.! "a"
  where
    initSignals = M.fromList [("b", part1 input)]

type Label = String

data Wire = WInt Word16 | WStr Label

data Op = AND Wire Wire | OR Wire Wire | LSHIFT Wire Int | RSHIFT Wire Int | NOT Wire | DIRECT Wire

type Signals = M.Map Label Op

type Cache = M.Map Label Word16

processSignals :: Signals -> Cache -> Cache
processSignals m cache = foldl processSignal cache (M.keys m)
  where
    processSignal cache' key
      | M.member key cache' = cache'
      | otherwise = M.insert key findIntVal cache'
      where
        findIntVal = case m M.! key of
          DIRECT w -> evalWire w
          AND w1 w2 -> evalWire w1 .&. evalWire w2
          OR w1 w2 -> evalWire w1 .|. evalWire w2
          LSHIFT w i -> evalWire w `shift` i
          RSHIFT w i -> evalWire w `shiftR` i
          NOT w -> complement (evalWire w)
        evalWire (WInt i) = i
        evalWire (WStr s) = processSignals m cache' M.! s

parseSignals :: T.Text -> Signals
parseSignals input = M.fromList $ parseAoCInput input signalsParser "signalsParser"
  where
    numParser = read <$> P.many1 P.digit
    word16Parser = read <$> P.many1 P.digit
    labelParser = P.many1 P.letter
    wireParser = P.choice $ P.try <$> [WInt <$> word16Parser, WStr <$> labelParser]
    arrowParser = P.string " -> "
    directParser = DIRECT <$> (wireParser <* arrowParser)
    notParser = NOT <$> (P.string "NOT " *> wireParser <* arrowParser)
    andParser = AND <$> (wireParser <* P.string " AND ") <*> (wireParser <* arrowParser)
    orParser = OR <$> (wireParser <* P.string " OR ") <*> (wireParser <* arrowParser)
    lShiftParser = LSHIFT <$> (wireParser <* P.string " LSHIFT ") <*> (numParser <* arrowParser)
    rShiftParser = RSHIFT <$> (wireParser <* P.string " RSHIFT ") <*> (numParser <* arrowParser)
    opParser = P.choice $ P.try <$> [directParser, notParser, andParser, orParser, lShiftParser, rShiftParser]
    signalParser = flip (,) <$> opParser <*> labelParser
    signalsParser = P.many1 $ signalParser <* P.optional P.newline
