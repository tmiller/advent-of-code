module Day2 where

import qualified Data.MultiSet as M
import qualified Data.Set as S

input :: String
input = "data/day2.txt"

part1 :: IO String
part1 = show
     <$> product
     <$> counts
     <$> mconcat
     <$> (fmap . fmap) getReps parseFile

part2 :: IO String
part2 = undefined

getReps :: String -> M.MultiSet Reps
getReps = occurToReps
        . dedup
        . counts
        . dist

parseFile :: IO [String]
parseFile = lines <$> readFile input

data Reps = Dupes
          | Trips
          deriving (Show, Ord, Eq)

dist :: String -> M.MultiSet Char
dist = M.fromList

counts :: M.MultiSet a -> [M.Occur]
counts = fmap snd . M.toOccurList

dedup :: [M.Occur] -> [M.Occur]
dedup = S.toList . S.fromList

occurToReps :: [M.Occur] -> M.MultiSet Reps
occurToReps [] = M.empty
occurToReps (2:xs) = M.insert Dupes (occurToReps xs)
occurToReps (3:xs) = M.insert Trips (occurToReps xs)
occurToReps (_:xs) = occurToReps xs
