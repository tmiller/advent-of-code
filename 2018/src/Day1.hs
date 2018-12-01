module Day1 where

import Text.Trifecta
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Set as S
import Data.Bool (bool)

input :: String
input = "data/day1.txt"

part1 :: IO String
part1 =  mconcat <$> getNumbers >>= display

part2 :: IO String
part2 = findDuplicate <$> scan <$> cycle <$> getNumbers >>= display
  where scan = scanl (<>) (Sum 0)

getNumbers :: IO [Sum Integer]
getNumbers = fromMaybe [] <$> parseFromFile p input
  where p = ((Sum <$> integer) `sepBy` whiteSpace) <* eof


findDuplicate :: (Monoid a, Ord a) => [a] -> a
findDuplicate xs = go xs S.empty
  where go []      _    = mempty
        go (x:xs') seen = bool (go xs' (S.insert x seen)) x (S.member x seen)

display :: Sum Integer -> IO String
display = return . show . getSum
