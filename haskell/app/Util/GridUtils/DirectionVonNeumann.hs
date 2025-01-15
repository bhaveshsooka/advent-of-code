module Util.GridUtils.DirectionVonNeumann where

data Direction
  = N
  | E
  | S
  | W
  deriving (Show, Eq)

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N
