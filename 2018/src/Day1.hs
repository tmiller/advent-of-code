module Day1 where

import Text.Read
import Data.Monoid
import Lib

input :: String
input = "data/day1.txt"

part1 :: IO String
part1 = show <$> mconcat <$> getNumbers

part2 :: IO String
part2 = getNumbers
    ||> cycle
    ||> scanl (<>) (Just (Sum 0))
    ||> findDuplicate
    ||> show

getNumbers :: IO [Maybe (Sum Integer)]
getNumbers = readFile input
         ||> lines
         ||> fmap parseInt

parseInt :: String -> Maybe (Sum Integer)
parseInt ('+' : xs) = Sum <$> readMaybe xs
parseInt ('-' : xs) = Sum <$> negate <$> readMaybe xs
parseInt _          = Nothing

findDuplicate :: Eq a => [Maybe a] -> Maybe a
findDuplicate xs = go xs []
  where go []     _  = Nothing
        go (x:xs') ys = case elem x ys of
                         True  -> x
                         False -> go xs' (x:ys)
