module AOC2018.Day03
  ( part1,
    part2,
  )
where

import Data.HashSet qualified as H
import Data.Map qualified as M
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.GridUtils.Coord (Coord (Coord))
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = H.size $ H.unions overlaps
  where
    overlaps = (\((_, v1), (_, v2)) -> v1 `H.intersection` v2) <$> blockPairs
    blockPairs = [(a, b) | (i1, a) <- blocks, (i2, b) <- blocks, i1 < i2]
    blocks :: [(Int, (ID, H.HashSet Coord))] = zip [1 ..] $ generateBlock <$> fabricRectangles
    fabricRectangles = parseFabricRectangles input

part2 :: T.Text -> M.Map ID Int
part2 input = M.filter (\v -> v == length fabricRectangles - 1) idCounts
  where
    idCounts = countIds emptyOverlaps M.empty
    emptyOverlaps = filter (\(_, _, e) -> H.size e == 0) overlaps
    overlaps = (\((id1, v1), (id2, v2)) -> (id1, id2, v1 `H.intersection` v2)) <$> blockPairs
    blockPairs = [(a, b) | (i1, a) <- blocks, (i2, b) <- blocks, i1 < i2]
    blocks :: [(Int, (ID, H.HashSet Coord))] = zip [1 ..] $ generateBlock <$> fabricRectangles
    fabricRectangles = parseFabricRectangles input

type RectangleSize = (Int, Int)

type ID = Int

data FabricRectangle = FabricRectangle ID Coord RectangleSize deriving (Show)

countIds :: [(ID, ID, H.HashSet Coord)] -> M.Map ID Int -> M.Map ID Int
countIds [] acc = acc
countIds ((id1, id2, _) : xs) acc = countIds xs m2
  where
    m1 = M.insertWith (+) id1 1 acc
    m2 = M.insertWith (+) id2 1 m1

generateBlock :: FabricRectangle -> (ID, H.HashSet Coord)
generateBlock (FabricRectangle i (Coord startX startY) (rows, cols)) =
  ( i,
    H.fromList
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
