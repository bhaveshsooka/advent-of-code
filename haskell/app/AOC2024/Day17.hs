module AOC2024.Day17
  ( part1,
    part2,
  )
where

{-
64751475
37221270076916
-}

import Data.Bits (Bits (xor))
import Data.List (singleton, (!?))
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> String
part1 input = fmtProgOutput (\x -> take (length x - 1) x) foldFn "," "" $ processProgram registers [] 0 program
  where
    (registers, program) = parseRegistersAndProgram input
    foldFn o = case o of (Output output) -> show output; _ -> ""

part2 :: T.Text -> Int
part2 input = _BruteForceFindARegister registers program initialA
  where
    (Registers (Register initialA) _ _, program) = parseRegistersAndProgram input
    registers = Registers (Register 37221270076915) (Register 0) (Register 0)

newtype Register = Register Int deriving (Show)

data Registers = Registers Register Register Register deriving (Show)

type OpCode = Int

type Operand = Int

data OperandType = Literal | Combo deriving (Show)

data ProgOutput
  = NewRegisters Registers
  | Output Int
  | Jump Int
  | NoOutput
  deriving (Show)

_BruteForceFindARegister :: Registers -> [Int] -> Int -> Int
_BruteForceFindARegister registers@(Registers (Register a) (Register b) (Register c)) program initialA
  | length programOutput /= length program =
      _BruteForceFindARegister (Registers (Register $ a + 1) (Register b) (Register c)) program initialA
  | program == fmtProgOutput id foldFn [] [] programOutput && a /= initialA = a
  | otherwise = _BruteForceFindARegister (Registers (Register $ a + 1) (Register b) (Register c)) program initialA
  where
    programOutput = processProgram registers [] 0 program
    foldFn o = case o of (Output output) -> singleton output; _ -> []

fmtProgOutput :: (Semigroup (f a)) => (f a -> f a) -> (b -> f a) -> f a -> f a -> [b] -> f a
fmtProgOutput c f d s p = c $ foldl (\acc x -> acc <> f x <> d) s p

processProgram :: Registers -> [ProgOutput] -> Int -> [Int] -> [ProgOutput]
processProgram registers progOutput ptr program =
  case (program !? ptr, program !? (ptr + 1)) of
    (Just opCode, Just operand) -> case processOpCode registers opCode operand of
      NoOutput -> processProgram registers progOutput (ptr + 2) program
      Output output -> processProgram registers (progOutput ++ [Output output]) (ptr + 2) program
      NewRegisters newRegisters -> processProgram newRegisters progOutput (ptr + 2) program
      Jump newPtr -> processProgram registers progOutput newPtr program
    _ -> progOutput

getOperandValue :: Registers -> Operand -> OperandType -> Int
getOperandValue _ operand Literal = operand
getOperandValue (Registers (Register a) (Register b) (Register c)) operand Combo
  | operand > 0 && operand < 4 = operand
  | operand == 4 = a
  | operand == 5 = b
  | operand == 6 = c
  | operand == 7 = error "Reserved"
  | otherwise = error "Invalid operand"

processOpCode :: Registers -> OpCode -> Operand -> ProgOutput
processOpCode registers@(Registers (Register a) (Register b) (Register c)) opCode operand = case opCode of
  0 -> do
    let numerator = a
    let denominator = 2 ^ getOperandValue registers operand Combo
    let result = numerator `div` denominator
    NewRegisters $ Registers (Register result) (Register b) (Register c)
  1 -> do
    let result = b `xor` getOperandValue registers operand Literal
    NewRegisters $ Registers (Register a) (Register result) (Register c)
  2 -> do
    let result = getOperandValue registers operand Combo `mod` 8
    NewRegisters $ Registers (Register a) (Register result) (Register c)
  3 ->
    if a == 0
      then NoOutput
      else Jump $ getOperandValue registers operand Literal
  4 -> do
    let result = b `xor` c
    NewRegisters $ Registers (Register a) (Register result) (Register c)
  5 -> Output $ getOperandValue registers operand Combo `mod` 8
  6 -> do
    let numerator = a
    let denominator = 2 ^ getOperandValue registers operand Combo
    let result = numerator `div` denominator
    NewRegisters $ Registers (Register a) (Register result) (Register c)
  7 -> do
    let numerator = a
    let denominator = 2 ^ getOperandValue registers operand Combo
    let result = numerator `div` denominator
    NewRegisters $ Registers (Register a) (Register b) (Register result)
  _ -> error "Invalid opCode"

parseRegistersAndProgram :: T.Text -> (Registers, [Int])
parseRegistersAndProgram input = parseAoCInput input registersAndProgramParser "registersAndProgramParser"
  where
    numParser = read <$> P.many1 P.digit
    numParserWithVoider voidStr = P.string voidStr *> numParser
    registerParser lab = Register <$> numParserWithVoider ("Register " <> lab <> ": ")
    registersParser =
      Registers
        <$> (registerParser "A" <* P.newline)
        <*> (registerParser "B" <* P.newline)
        <*> (registerParser "C" <* P.newline)
    programParser = P.string "Program: " *> P.sepBy numParser (P.char ',')
    registersAndProgramParser = (,) <$> registersParser <* P.newline <*> programParser
