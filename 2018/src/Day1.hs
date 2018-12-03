module Day1 where

import Data.Bool (bool)
import Data.Monoid
import Data.Set as S
import Lib
import Text.Megaparsec

input :: String
input = "data/day1.txt"

part1 :: IO String
part1 = eitherPE (show . getSum . mconcat) <$> parseFile

part2 :: IO String
part2 = eitherPE (show . getSum . findDuplicate . scan . cycle) <$> parseFile
  where scan = scanl (<>) (Sum 0)

parseFile :: IO (Either ParseErrorBundle [Sum Integer])
parseFile = parse (many (Sum <$> integer) <* eof) input <$> readFile input

findDuplicate :: (Monoid a, Ord a) => [a] -> a
findDuplicate = go S.empty
  where go _ []                    = mempty
        go s (x:xs) | S.member x s = x
                    | otherwise    = go (S.insert x s) xs
