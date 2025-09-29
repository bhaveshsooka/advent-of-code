module AOC2015.Day16
  ( part1,
    part2,
  )
where

import Data.HashMap.Internal.Strict qualified as H
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> [Int]
part1 input = fst <$> filter (snd . isGifter) (parseAunts input)

part2 :: T.Text -> [Int]
part2 input = fst <$> filter (snd . isRealGifter) (parseAunts input)

type Aunt = (Int, [(String, Int)])

type Aunts = [Aunt]

type Memory = H.HashMap String Int

memory :: Memory
memory =
  H.fromList
    [ ("children", 3),
      ("cats", 7),
      ("samoyeds", 2),
      ("pomeranians", 3),
      ("akitas", 0),
      ("vizslas", 0),
      ("goldfish", 5),
      ("trees", 3),
      ("cars", 2),
      ("perfumes", 1)
    ]

isRealGifter :: Aunt -> (Int, Bool)
isRealGifter (i, items) = (i, go items)
  where
    go [] = True
    go ((k, v) : rest) =
      case k of
        "cats" -> (v > memory H.! k) && go rest
        "trees" -> (v > memory H.! k) && go rest
        "pomeranians" -> (v < memory H.! k) && go rest
        "goldfish" -> (v < memory H.! k) && go rest
        _ -> (v == memory H.! k) && go rest

isGifter :: Aunt -> (Int, Bool)
isGifter (i, items) = (i, go items)
  where
    go [] = True
    go ((k, v) : rest) = (v == memory H.! k) && go rest

parseAunts :: T.Text -> Aunts
parseAunts input = parseAoCInput input auntsParser "auntsParser"
  where
    numParser = read <$> P.many1 P.digit
    idParser = P.string "Sue " *> numParser <* P.string ": "
    itemParser = (,) <$> (P.many1 P.letter <* P.string ": ") <*> numParser
    itemsParser = itemParser `P.sepBy` P.string ", "
    auntParser = (,) <$> idParser <*> itemsParser
    auntsParser = P.many1 $ auntParser <* P.optional P.newline
