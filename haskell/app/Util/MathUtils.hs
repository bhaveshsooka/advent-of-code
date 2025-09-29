module Util.MathUtils where

factors :: (Integral a) => a -> [a]
factors n = [x | x <- [1 .. n], n `mod` x == 0]
