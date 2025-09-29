module AOC2015.Day15
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Score
part1 input = maximum (fst <$> cookieScores)
  where
    cookieScores = cookieScore ingredients <$> portions
    portions = compositionsNonNeg 100 (fromIntegral $ length ingredients)
    ingredients = parseIngredients input

part2 :: T.Text -> Score
part2 input = maximum (fst <$> filter ((== 500) . snd) cookieScores)
  where
    cookieScores = cookieScore ingredients <$> portions
    portions = compositionsNonNeg 100 (fromIntegral $ length ingredients)
    ingredients = parseIngredients input

type Ingredient = (String, Integer, Integer, Integer, Integer, Integer)

type Ingredients = [Ingredient]

type Portion = [Integer]

type Score = Integer

type Calories = Integer

compositionsNonNeg :: Integer -> Integer -> [[Integer]]
compositionsNonNeg n k
  | k == 0 = [[] | n == 0]
  | otherwise = [x : xs | x <- [0 .. n], xs <- compositionsNonNeg (n - x) (k - 1)]

cookieScore :: Ingredients -> Portion -> (Score, Calories)
cookieScore ingredients portion = tupleProduct $ foldr1 pairWiseScore portionVals
  where
    tupleProduct (c, d, f, t, cals) = if any (< 0) [c, d, f, t, cals] then (0, 0) else (c * d * f * t, cals)
    pairWiseScore (c1, d1, f1, t1, cal1) (c2, d2, f2, t2, cal2) = (c1 + c2, d1 + d2, f1 + f2, t1 + t2, cal1 + cal2)
    portionVals = zipWith (\(_, c, d, f, t, cals) p -> (c * p, d * p, f * p, t * p, cals * p)) ingredients portion

parseIngredients :: T.Text -> Ingredients
parseIngredients input = parseAoCInput input ingredientsParser "ingredientsParser"
  where
    numParser =
      P.choice
        [ read <$> P.many1 P.digit,
          negate . read <$> (P.char '-' *> P.many1 P.digit)
        ]
    nameParser = P.many1 P.letter <* P.string ": "
    capacityParser = P.string "capacity " *> numParser <* P.string ", "
    durabilityParser = P.string "durability " *> numParser <* P.string ", "
    flavorParser = P.string "flavor " *> numParser <* P.string ", "
    textureParser = P.string "texture " *> numParser <* P.string ", "
    caloriesParser = P.string "calories " *> numParser
    ingredientParser =
      (,,,,,)
        <$> nameParser
        <*> capacityParser
        <*> durabilityParser
        <*> flavorParser
        <*> textureParser
        <*> caloriesParser
    ingredientsParser = P.many1 $ ingredientParser <* P.optional P.newline
