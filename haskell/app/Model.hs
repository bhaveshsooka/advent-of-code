module Model (
  AOC_Day,
  Part (Part),
  Parts,
) where

import Data.Text qualified as T

type Year = Int
type Day = Int
type AOC_Day = (Year, Day)

data Part = forall a. (Show a) => Part (T.Text -> a)
type Parts = (Part, Part)
