module AOC2016.Day09
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector.Primitive qualified as V

part1 :: T.Text -> Int
part1 input = length $ findDecompressed input

part2 :: T.Text -> Int
part2 input = findDecompressed' (T.unpack input)

findDecompressed' :: String -> Int
findDecompressed' compressedStr = go 0 initMultipliers 0
  where
    initMultipliers = V.generate (length compressedStr) (const 1)

    go decompLen multipliers pos
      | pos >= length compressedStr = decompLen
      | compressedStr !! pos == '(' = go decompLen newMultipliers newPos
      | otherwise = go (decompLen + currentMultiplier) multipliers (pos + 1)
      where
        newMultipliers = foldl foldFn multipliers i's
        foldFn acc i = acc V.// [(i, currentMultiplier * numRepeats)]
        i's = [i | i <- [newPos .. newPos + numChars - 1], i < length compressedStr]
        currentMultiplier = multipliers V.! pos
        newPos = pos + length markerStr + 2
        numChars = read $ takeWhile (/= 'x') markerStr
        numRepeats = read $ drop 1 $ dropWhile (/= 'x') markerStr
        markerStr = takeWhile (/= ')') $ drop (pos + 1) compressedStr

findDecompressed :: T.Text -> String
findDecompressed compressedStr = go "" "" False (T.unpack compressedStr)
  where
    go decompressedStr _ _ [] = decompressedStr
    go decompressedStr markerStr isMarker (c : cs) =
      case (c, isMarker) of
        ('(', _) -> go decompressedStr markerStr True cs
        (')', _) -> go newDecompressedStr "" False $ drop numChars cs
        (_, True) -> go decompressedStr (markerStr <> [c]) isMarker cs
        (_, False) -> go (decompressedStr <> [c]) markerStr isMarker cs
      where
        newDecompressedStr = decompressedStr <> replicatedStr
        replicatedStr = concat (replicate numRepeats (take numChars cs))
        numChars = read $ takeWhile (/= 'x') markerStr
        numRepeats = read $ drop 1 $ dropWhile (/= 'x') markerStr
