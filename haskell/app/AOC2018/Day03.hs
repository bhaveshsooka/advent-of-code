module AOC2018.Day03
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Coord (Coord))
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = M.size $ M.filter (> 1) coordCounts
  where
    coordCounts = countPerCoord blocks M.empty
    blocks :: [(ID, [Coord])] = generateBlock <$> fabricRectangles
    fabricRectangles = parseFabricRectangles input

part2 :: T.Text -> M.Map ID Bool
part2 input = M.filter not $ isOverlapPerId blocks overlaps M.empty
  where
    overlaps = M.filter (> 1) $ countPerCoord blocks M.empty
    blocks :: [(ID, [Coord])] = generateBlock <$> fabricRectangles
    fabricRectangles = parseFabricRectangles input

type RectangleSize = (Int, Int)

type ID = Int

data FabricRectangle = FabricRectangle ID Coord RectangleSize deriving (Show)

countPerCoord :: [(ID, [Coord])] -> M.Map Coord Int -> M.Map Coord Int
countPerCoord [] coordMemo = coordMemo
countPerCoord ((_, blk) : xs) coordMemo = countPerCoord xs newAcc
  where
    newAcc = foldr (\c acc -> M.insertWith (+) c 1 acc) coordMemo blk

isOverlapPerId :: [(ID, [Coord])] -> M.Map Coord Int -> M.Map ID Bool -> M.Map ID Bool
isOverlapPerId [] _ acc = acc
isOverlapPerId ((cid, blk) : xs) overlaps idMemo =
  if any (`M.member` overlaps) blk
    then isOverlapPerId xs overlaps (M.insertWith (||) cid True idMemo)
    else isOverlapPerId xs overlaps (M.insertWith (||) cid False idMemo)

generateBlock :: FabricRectangle -> (ID, [Coord])
generateBlock (FabricRectangle cid (Coord startX startY) (rows, cols)) =
  ( cid,
    [ Coord x y
      | x <- (+ startX) <$> [0 .. rows - 1],
        y <- (+ startY) <$> [0 .. cols - 1]
    ]
  )

parseFabricRectangles :: T.Text -> [FabricRectangle]
parseFabricRectangles input = parseAoCInput input fabricRectanglesParser "fabricRectanglesParser"
  where
    numParser = read <$> P.many1 P.digit
    idParser = P.char '#' *> numParser <* P.string " @ "
    coordParser = Coord <$> (numParser <* P.char ',') <*> (numParser <* P.string ": ")
    rectangleSizeParser = (,) <$> (numParser <* P.char 'x') <*> numParser
    fabricRectangleParser = FabricRectangle <$> idParser <*> coordParser <*> rectangleSizeParser
    fabricRectanglesParser = P.many1 $ fabricRectangleParser <* P.optional P.newline
