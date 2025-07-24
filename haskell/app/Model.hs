module Model
  ( AOCDayPart (AOCDayPart, AOCDayPartTiming),
    AOCDayParts,
    AOCPartsResult,
    Timing (Picosecond, Nanosecond, Microsecond, Millisecond, Second, Minute),
  )
where

import Data.Text qualified as T

data Timing
  = Picosecond Double
  | Nanosecond Double
  | Microsecond Double
  | Millisecond Double
  | Second Double
  | Minute Double

instance Show Timing where
  show (Picosecond t) = show t ++ "ps"
  show (Nanosecond t) = show t ++ "ns"
  show (Microsecond t) = show t ++ "μs"
  show (Millisecond t) = show t ++ "ms"
  show (Second t) = show t ++ "s"
  show (Minute t) = show t ++ "m"

data AOCDayPart
  = forall a. (Show a) => AOCDayPart (T.Text -> a)
  | forall a. (Show a) => AOCDayPartTiming (T.Text -> a) Timing

type AOCDayParts = (AOCDayPart, AOCDayPart)

type AOCPartsResult = Either String AOCDayParts
