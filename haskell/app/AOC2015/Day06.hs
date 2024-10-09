{-# LANGUAGE OverloadedStrings #-}

module AOC2015.Day06 (
  printAoC2015Day06Answer,
) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (
  MArray (newArray),
  STArray,
  getAssocs,
  readArray,
  writeArray,
 )
import Data.Functor (($>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Text.Parsec (
  Parsec,
  char,
  choice,
  digit,
  many1,
  newline,
  optional,
  runParser,
  space,
  string,
  try,
  (<|>),
 )

printAoC2015Day06Answer :: IO ()
printAoC2015Day06Answer = do
  input <- TIO.readFile "./data/2015-06.txt"
  let instructions = runParser parseInstructions () "parseInstructions" input
      errorHandler = error . show
  putStrLn "------ Day 06 ------"
  let part1 = either errorHandler solve instructions False
  putStrLn $ "part1: " ++ show part1
  let part2 = either errorHandler solve instructions True
  putStrLn $ "part2: " ++ show part2
  putStrLn ""

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data Block = Block Coord Coord deriving (Show, Eq, Ord)
data Instruction = Turn Operation Block | Toggle Block deriving (Show, Eq, Ord)
data Operation = On | Off deriving (Show, Eq, Ord)

parseOperation :: Parsec T.Text () Operation
parseOperation = choice $ try <$> [on, off]
 where
  on = string "on" $> On
  off = string "off" $> Off

parseCoord :: Parsec T.Text () Coord
parseCoord = Coord <$> (getDigit <* char ',') <*> getDigit
  where 
    getDigit = read <$> many1 digit

parseCoords :: Parsec T.Text () Block
parseCoords = Block <$> (parseCoord <* string " through ") <*> parseCoord

parseTurn :: Parsec T.Text () Instruction
parseTurn = Turn <$> (string "turn " *> parseOperation <* space) <*> parseCoords

parseToggle :: Parsec T.Text () Instruction
parseToggle = Toggle <$> (string "toggle " *> parseCoords)

parseInstruction :: Parsec T.Text () Instruction
parseInstruction = try parseTurn <|> try parseToggle

parseInstructions :: Parsec T.Text () [Instruction]
parseInstructions = many1 $ parseInstruction <* optional newline

solve :: [Instruction] -> Bool -> Int
solve instructions brightness = runST $ do
  grid <- emptyArray 1000 1000
  forM_ instructions (applyInstruction grid brightness)
  countOrSumLights brightness grid

countOrSumLights :: Bool -> STArray s (Int, Int) Int -> ST s Int
countOrSumLights brightness grid = do
  elements <- getAssocs grid
  if brightness
    then return $ sum $ map snd elements
    else return $ length $ filter (>= 1) $ map snd elements

emptyArray :: Int -> Int -> ST s (STArray s (Int, Int) Int)
emptyArray rows cols = newArray ((0, 0), (rows - 1, cols - 1)) 0

applyInstruction :: STArray s (Int, Int) Int -> Bool -> Instruction -> ST s ()
applyInstruction grid brightness instruction = do
  let (Coord blx bly, Coord trx try_) = getArrayBounds instruction
  forM_ [blx .. trx] $ \x ->
    forM_ [bly .. try_] $ \y -> do
      val <- readArray grid (x, y)
      writeArray grid (x, y) $ updateValue brightness val instruction

getArrayBounds :: Instruction -> (Coord, Coord)
getArrayBounds (Turn On (Block bl tr)) = (bl, tr)
getArrayBounds (Turn Off (Block bl tr)) = (bl, tr)
getArrayBounds (Toggle (Block bl tr)) = (bl, tr)

updateValue :: Bool -> Int -> Instruction -> Int
updateValue brightness val (Turn On _) = if brightness then val + 1 else 1
updateValue brightness val (Turn Off _) = if brightness then max 0 (val - 1) else 0
updateValue brightness val (Toggle _) = if brightness then val + 2 else toggle val

toggle :: Int -> Int
toggle 1 = 0
toggle 0 = 1
toggle x = x
