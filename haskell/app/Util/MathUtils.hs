module Util.MathUtils where

import Data.List (nub)

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

factors :: (Integral a) => a -> [a]
factors n = nub . concat $ [[x, q] | x <- [1 .. isqrt n], let (q, r) = divMod n x, r == 0]
