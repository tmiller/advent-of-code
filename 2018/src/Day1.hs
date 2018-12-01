module Day1 where

import Text.Read
import Data.Monoid
import Data.Set as S
import Data.Bool (bool)

input :: String
input = "data/day1.txt"

part1 :: IO String
part1 = show <$> mconcat <$> getNumbers

part2 :: IO String
part2 = show <$> findDuplicate <$> scan <$> cycle <$> getNumbers
  where scan = scanl (<>) (Just (Sum 0))

getNumbers :: IO [Maybe (Sum Integer)]
getNumbers = fmap parseInt <$> lines <$> readFile input

parseInt :: String -> Maybe (Sum Integer)
parseInt ('+' : xs) = Sum <$> readMaybe xs
parseInt ('-' : xs) = Sum <$> negate <$> readMaybe xs
parseInt _          = Nothing

findDuplicate :: Ord a => [Maybe a] -> Maybe a
findDuplicate xs = go xs S.empty
  where go []      _    = Nothing
        go (x:xs') seen = bool (go xs' (S.insert x seen)) x (S.member x seen)
