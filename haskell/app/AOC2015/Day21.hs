module AOC2015.Day21
  ( part1,
    part2,
  )
where

import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ListUtils (combinations)
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = snd . minimumBy (compare `on` snd) $ filter fst gameStates
  where
    gameStates = (\p -> runGameLoop Player (playerHP, p) (bossHP, (0, bossDamage, bossArmor))) <$> inventoryStats
    inventoryStats = calculateStats <$> inventoryOptions
    playerHP :: HP = 100
    (bossHP, bossDamage, bossArmor) = parseBossStats input

part2 :: T.Text -> Int
part2 input = snd . maximumBy (compare `on` snd) $ filter (not . fst) gameStates
  where
    gameStates = (\p -> runGameLoop Player (playerHP, p) (bossHP, (0, bossDamage, bossArmor))) <$> inventoryStats
    inventoryStats = calculateStats <$> inventoryOptions
    playerHP :: HP = 100
    (bossHP, bossDamage, bossArmor) = parseBossStats input

type Weapon = (String, Int, Int, Int)

type Defense = (String, Int, Int, Int)

type Ring = (String, Int, Int, Int)

type EquipmentSet = (Weapon, [Defense], [Ring])

type Cost = Int

type Damage = Int

type Armor = Int

type HP = Int

data Turn = Player | Boss deriving (Eq)

runGameLoop :: Turn -> (HP, (Cost, Damage, Armor)) -> (HP, (Cost, Damage, Armor)) -> (Bool, Cost)
runGameLoop turn p@(php, (pc, pd, pa)) b@(bhp, (bc, bd, ba))
  | php <= 0 = (False, pc)
  | bhp <= 0 = (True, pc)
  | turn == Player = runGameLoop Boss p (bhp - (pd - ba), (bc, bd, ba))
  | otherwise = runGameLoop Player (php - (bd - pa), (pc, pd, pa)) b

calculateStats :: EquipmentSet -> (Cost, Damage, Armor)
calculateStats ((_, wCost, wDamage, wArmor), ds, rs) = (defCost + wCost + ringCost, defDamage + wDamage + ringDamage, defArmor + wArmor + ringArmor)
  where
    (defCost, defDamage, defArmor) = foldr (\(_, dCost, dDamage, dArmor) (c, d, a) -> (c + dCost, d + dDamage, a + dArmor)) (0, 0, 0) ds
    (ringCost, ringDamage, ringArmor) = foldr (\(_, rCost, rDamage, rArmor) (c, d, a) -> (c + rCost, d + rDamage, a + rArmor)) (0, 0, 0) rs

inventoryOptions :: [EquipmentSet]
inventoryOptions =
  [(w, [d], [r]) | w <- weapons, d <- defenses, r <- rings] -- one ring selected
    ++ [(w, [d], rs) | w <- weapons, d <- defenses, rs <- combinations 2 rings] -- two rings selected
    ++ [(w, [d], []) | w <- weapons, d <- defenses] -- no rings selected
    ++ [(w, [], [r]) | w <- weapons, r <- rings] -- no defense selected, one ring selected
    ++ [(w, [], rs) | w <- weapons, rs <- combinations 2 rings] -- no defense selected, two rings selected
    ++ [(w, [], []) | w <- weapons] -- no defense selected, no rings selected

weapons :: [Weapon]
weapons =
  [ ("Dagger", 8, 4, 0),
    ("Shortsword", 10, 5, 0),
    ("Warhammer", 25, 6, 0),
    ("Longsword", 40, 7, 0),
    ("Greataxe", 74, 8, 0)
  ]

defenses :: [Defense]
defenses =
  [ ("Leather", 13, 0, 1),
    ("Chainmail", 31, 0, 2),
    ("Splintmail", 53, 0, 3),
    ("Bandedmail", 75, 0, 4),
    ("Platemail", 102, 0, 5)
  ]

rings :: [Ring]
rings =
  [ ("Damage +1", 25, 1, 0),
    ("Damage +2", 50, 2, 0),
    ("Damage +3", 100, 3, 0),
    ("Defense +1", 20, 0, 1),
    ("Defense +2", 40, 0, 2),
    ("Defense +3", 80, 0, 3)
  ]

parseBossStats :: T.Text -> (HP, Damage, Armor)
parseBossStats input = parseAoCInput input bossStatsParser "bossStatsParser"
  where
    numParser = read <$> P.many1 P.digit
    hpParser = P.string "Hit Points: " *> numParser <* P.newline
    damageParser = P.string "Damage: " *> numParser <* P.newline
    armorParser = P.string "Armor: " *> numParser
    bossStatsParser = (,,) <$> hpParser <*> damageParser <*> armorParser <* P.optional P.newline
