module AOC2015.Day20
  ( part1,
    part2,
  )
where

import Control.Monad.ST (runST)
import Data.Text qualified as T
import Data.Vector.Unboxed.Mutable qualified as M
import Data.Word (Word32)

part1 :: T.Text -> Int
part1 input = runST $ do
  let presentsTarget = (read . T.unpack $ input) `div` 10 :: Int
  houses <- M.replicate presentsTarget (0 :: Word32)
  let loopOuter !houseNum !i
        | i >= presentsTarget = pure houseNum
        | otherwise = do
            let inner !j !best
                  | j >= presentsTarget = pure best
                  | otherwise = do
                      v <- M.unsafeRead houses j
                      let !newV = v + fromIntegral i
                      M.unsafeWrite houses j newV
                      let !best' = if newV >= fromIntegral presentsTarget && j < best then j else best
                      inner (j + i) best'
            best' <- inner i houseNum
            loopOuter best' (i + 1)
  loopOuter presentsTarget 1

part2 :: T.Text -> Int
part2 input = runST $ do
  let presentsTarget = (read . T.unpack $ input) `div` 10 :: Int
      thr = presentsTarget * 10
  houses <- M.replicate presentsTarget (0 :: Word32)

  let outer !best !i
        | i >= presentsTarget = pure best
        | otherwise = do
            best' <- inner best i (0 :: Int) i -- best, j, visits, step=i
            outer best' (i + 1)

      inner !best !j !visits !step
        | j >= presentsTarget = pure best
        | visits >= 50 = pure best
        | otherwise = do
            v <- M.unsafeRead houses j
            let !base = if v == 0 then 11 else v
                !add = fromIntegral step * 11
                !newV = base + add
            M.unsafeWrite houses j newV
            let !best' = if fromIntegral newV >= thr && j < best then j else best
            inner best' (j + step) (visits + 1) step

  outer presentsTarget 1
