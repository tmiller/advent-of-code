module Main where

import qualified Day1

main :: IO ()
main = do
  d1p1 <- Day1.part1
  putStrLn ("Day01 Part1: " ++ d1p1)
  --d1p2 <- Day1.part2
  --putStrLn ("Day01 Part2: " ++ d1p2)
