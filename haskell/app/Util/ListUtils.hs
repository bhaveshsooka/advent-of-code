module Util.ListUtils
  ( combinations,
  )
where

-- implementation of N choose K combinations
-- combinations 2 [1,2,3] -> [[1,2],[1,3],[2,3]]
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs
