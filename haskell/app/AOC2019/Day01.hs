module AOC2019.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import GHC.Float (floorDouble)
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr ((+) . calcFuel) 0 $ parseModules input
  where
    calcFuel n = floorDouble (n / 3) - 2

part2 :: T.Text -> Int
part2 input = foldr (\e acc -> acc + ff e) 0 $ parseModules input
  where
    calcFuel n = floorDouble (n / 3) - 2
    ff e = calcFuelForFuel (calcFuel e) (floorDouble e) 0

calcFuelForFuel :: Int -> Int -> Int -> Int
calcFuelForFuel f m acc =
  if fuelForF <= 0
    then acc + fuelForM
    else calcFuelForFuel fuelForF m (acc + fuelForF)
  where
    fuelForF = (f `div` 3) - 2
    fuelForM = (m `div` 3) - 2

parseModules :: T.Text -> [Double]
parseModules input = parseAoCInput input modulesParser "modulesParser"
  where
    moduleParser = read <$> P.many1 P.digit
    modulesParser = P.many1 $ moduleParser <* P.optional P.newline
