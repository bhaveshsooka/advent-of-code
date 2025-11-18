module AOC2020.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = length . filter isPasswordValidOld $ parsePasswordPolicies input

part2 :: T.Text -> Int
part2 input = length . filter isPasswordValidNew $ parsePasswordPolicies input

type Min = Int

type Max = Int

data PasswordRule = PasswordRule Min Max Char deriving (Show)

type Password = String

type PasswordPolicy = (PasswordRule, Password)

isPasswordValidNew :: PasswordPolicy -> Bool
isPasswordValidNew (PasswordRule pos1 pos2 c, pwd) = (a || b) && not (a && b)
  where
    a = c == pwd !! (pos1 - 1)
    b = c == pwd !! (pos2 - 1)

isPasswordValidOld :: PasswordPolicy -> Bool
isPasswordValidOld (PasswordRule mn mx c, pwd) = mn <= cnt && cnt <= mx
  where
    cnt = length . filter (== c) $ pwd

parsePasswordPolicies :: T.Text -> [PasswordPolicy]
parsePasswordPolicies input = parseAoCInput input passwordPoliciesParser "passwordPoliciesParser"
  where
    numParser = read <$> P.many1 P.digit
    passwordRuleParser =
      PasswordRule
        <$> (numParser <* P.char '-')
        <*> (numParser <* P.space)
        <*> (P.anyChar <* P.string ": ")
    passwordPolicyParser = (,) <$> passwordRuleParser <*> P.many1 P.letter
    passwordPoliciesParser = P.many1 $ passwordPolicyParser <* P.optional P.newline
