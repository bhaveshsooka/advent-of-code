{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day09 (
  printAoC2015Day09Answer,
) where

import Data.Map (Map, empty, insert, member, (!))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Parsec (Parsec, digit, letter, many1, newline, optional, runParser, string)

type City = String
type Distance = Int
type Edge = (City, City)
type G = Map City ([(City, Distance)])

parseLine :: Parsec T.Text () (Edge, Distance)
parseLine = do
  city1 <- many1 letter <* string " to "
  city2 <- many1 letter <* string " = "
  distance <- read <$> many1 digit
  return ((city1, city2), distance)

parseInput :: Parsec T.Text () [(Edge, Distance)]
parseInput = many1 $ parseLine <* optional newline

createGraph :: [(Edge, Distance)] -> G
createGraph = foldr (\(a, b) g -> insertEdge a b g) empty

insertEdge :: Edge -> Distance -> G -> G
insertEdge (a, b) d g
  | not $ member a g = insert a [(b, d)] g
  | otherwise = insert a ((b, d) : filter ((/= b) . fst) (g ! a)) g

printAoC2015Day09Answer :: IO ()
printAoC2015Day09Answer = do
  input <- TIO.readFile "./data/2015-09.txt"
  let edges = runParser parseInput () "parseInput" input
  let g = either (error . show) createGraph edges
  putStrLn $ show g
  putStrLn "------ Day 09 ------"
  -- let part1 = (sum fullLength) - (sum innerLength)
  -- putStrLn $ "part1: " ++ show part1
  -- let part2 = (sum inverseInnerLength) - (sum fullLength)
  -- putStrLn $ "part2: " ++ show part2
  putStrLn ""
