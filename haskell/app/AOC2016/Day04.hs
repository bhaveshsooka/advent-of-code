module AOC2016.Day04
  ( part1,
    part2,
  )
where

import Data.List (elemIndex, find, isPrefixOf, nub, sortBy)
import Data.Ord (comparing)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\(Room _ sectorId _) acc -> acc + sectorId) 0 (filter isRealRoom (parseRooms input))

part2 :: T.Text -> Int
part2 input = case northPoleObjectStorageResult of
  Just (_, sectorId) -> sectorId
  Nothing -> -1
  where
    northPoleObjectStorageResult = find (\(name, _) -> name == "northpoleobjectstorage") decryptedRoomNames
    decryptedRoomNames = (\(Room name sectorId _) -> (rotateLetter sectorId <$> name, sectorId)) <$> realRooms
    realRooms = filter isRealRoom $ parseRooms input

type Name = String

type SectorId = Int

type Checksum = String

data Room = Room Name SectorId Checksum deriving (Show)

rotateLetter :: Int -> Char -> Char
rotateLetter sectorId c = alphabet !! newPos
  where
    newPos = case newPosResult of
      Just i -> (i + sectorId) `mod` 26
      Nothing -> -1
    newPosResult = elemIndex c alphabet
    alphabet = ['a' .. 'z']

isRealRoom :: Room -> Bool
isRealRoom (Room name _ checksum) = checksum `isPrefixOf` strFrequencies
  where
    strFrequencies = fst <$> sortedFrequencies
    sortedFrequencies = sortBy (flip (comparing snd) <> comparing fst) uniqueFrequencies
    uniqueFrequencies = nub $ (\c -> (c, occurrance c name)) <$> name
    occurrance a = length . filter (a ==)

parseRooms :: T.Text -> [Room]
parseRooms input = parseAoCInput input roomsParser "roomsParser"
  where
    nameParser = concat <$> P.many1 (P.many1 P.letter <* P.optional (P.char '-'))
    sectorIdParser = read <$> P.many1 P.digit
    checksumParser = P.char '[' *> P.many1 P.letter <* P.char ']'
    roomParser = Room <$> nameParser <*> sectorIdParser <*> checksumParser
    roomsParser = P.many1 $ roomParser <* P.optional P.newline
