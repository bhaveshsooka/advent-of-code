{-# LANGUAGE ImpredicativeTypes #-}

module Model
  ( AOCDayPart (AOCDayPart),
    AOCDayParts,
    AOCPartsResult,
  )
where

import Data.Text qualified as T

data AOCDayPart = forall a. (Show a) => AOCDayPart (T.Text -> a)

type AOCDayParts = (AOCDayPart, AOCDayPart)

type AOCPartsResult = Either String AOCDayParts
