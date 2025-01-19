module AOC2024.Day11
  ( part1,
    part2,
  )
where

import Data.HashMap.Strict qualified as M
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ foldr (\_ -> concatMap applyRules) stones blinks
  where
    blinks :: [Int] = [1 .. 25]
    stones = read . T.unpack <$> T.splitOn (T.pack " ") input

part2 :: T.Text -> Int
part2 input = foldr foldFn 0 memo
  where
    foldFn (Memo steps _ _) acc = if (steps !! (numBlinks - 1)) /= (-1) then acc + (steps !! (numBlinks - 1)) else acc
    (_, memo) = foldl blinkMFMemoFold (stones, createMemoTable (length blinks) M.empty stones) blinks
    blinks :: [(Int, Int)] = zip (replicate numBlinks numBlinks) [0 .. numBlinks - 1]
    numBlinks = 75
    createMemoTable n = foldr (\x acc -> addToMemo n x 0 acc)
    addToMemo blinks' stone offset = M.insert stone (Memo (replicate blinks' (-1)) offset [])
    stones = read . T.unpack <$> T.splitOn (T.pack " ") input

type Steps = [Int]

type Offset = Int

type Children = [Int]

data Memo = Memo Steps Offset Children deriving (Show)

blinkMFMemoFold :: ([Int], M.HashMap Int Memo) -> (Int, Offset) -> ([Int], M.HashMap Int Memo)
blinkMFMemoFold (stones, memoCache) (blinks, t)
  | t == blinks = (stones, memoCache)
  | otherwise = (foldr addToStonesFold stones updatedStones, updatedMemo)
  where
    addToStonesFold x acc = if x `elem` acc then acc else acc ++ [x]
    (updatedMemo, updatedStones) = foldr newStonesFold (memoCache, []) stones
    newStonesFold stone (memo, newStones) =
      maybe
        (memo, newStones)
        (newStonesMaybeFn stone newStones memo)
        (M.lookup stone memo)
    newStonesMaybeFn stone newStones memo (Memo steps offset stonesChildren) =
      case stonesChildren of
        [] -> (newNewMemo, newNewStones)
          where
            newNewMemo = foldr addChildToMemo memoWithChildren childrenNotInMemo
            memoWithChildren = M.insert stone (Memo newSteps offset children) memo
            childrenNotInMemo = foldr childrenNotInMemoFold [] children
            childrenNotInMemoFold child acc = if M.member child memo then acc else child : acc
            addChildToMemo child = M.insert child (Memo (replicate blinks (-1)) (t + 1) [])
            newNewStones = newStones ++ children
            newSteps = length children : drop 1 steps
            children = applyRules stone
        _ -> (memoWithChildren, newStones)
          where
            memoWithChildren = M.insert stone (Memo stoneNewStepsVal offset stonesChildren) memo
            stoneNewStepsVal = take (t - offset) steps ++ [sumOfChildren] ++ drop (t - offset + 1) steps
            sumOfChildren = foldr sumOfChildrenFold 0 stonesChildren
            sumOfChildrenFold child childSum = childSum + maybe 0 maybeChildInMemo (M.lookup child memo)
            maybeChildInMemo (Memo s _ _) = s !! (t - offset - 1)

applyRules :: Int -> [Int]
applyRules stone
  | stone == 0 = [1]
  | even (length (show stone)) = [read firstHalf, read secondHalf]
  | otherwise = [stone * 2024]
  where
    firstHalf = take (n `div` 2) strStone
    secondHalf = drop (n `div` 2) strStone
    n = length strStone
    strStone = show stone
