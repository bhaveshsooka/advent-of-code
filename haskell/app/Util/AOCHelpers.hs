module Util.AOCHelpers (
  printDay,
  Parts,
  Part (Part),
) where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

type Year = Int
type Day = Int
type AOC_Day = (Year, Day)

data Part = forall a. (Show a) => Part (T.Text -> a)
type Parts = (Part, Part)

printDay :: AOC_Day -> Parts -> IO ()
printDay (year, d) ((Part p1), (Part p2)) = do
  let day = padLeft (T.pack $ show d) '0' 2
      filename = "./data/" <> show year <> "-" <> T.unpack day <> ".txt"
  input <- TIO.readFile filename
  putStrLn $ "------ Day " <> show day <> " ------"
  let part1 = show $ p1 input
  putStrLn $ "part1: " <> part1
  let part2 = show $ p2 input
  putStrLn $ "part2: " <> part2
  putStrLn ""

padLeft :: T.Text -> Char -> Int -> T.Text
padLeft s c len = T.replicate (len - T.length s) (T.singleton c) <> s
