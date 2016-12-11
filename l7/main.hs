module Main
where

import System.IO
import Data.List (groupBy, sortBy)
import Data.Function
import Data.Ord

main :: IO ()
main =
  withFile "input.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let packets = lines contents
    putStrLn $ show $ length $ filter (validatePacket . generatePacket) packets
  )

type Segment = (Bool, String)

validatePacket :: [Segment] -> Bool
validatePacket packet =
  hasAbba && hasNoAbba
  where
    hasAbba = or $ map (containsAbba . snd) confirmAbba
    hasNoAbba = and $ map (not . containsAbba . snd) confirmNoAbba
    (confirmNoAbba:confirmAbba:[]) = sortedPacket
    sortedPacket = groupBy ((==) `on` fst) $ sortBy (comparing fst) packet
  

generatePacket :: String -> [Segment]
generatePacket =
  g' True
  where
    g' _     []        = []
    g' state list
      | elem ']' list  = [(state, listHead)] ++ (g' (not state) listTail)
      | otherwise      = [(state, listHead)]
      where
        listHead = (takeWhile ((flip notElem) "[]") list)
        listTail = tail (dropWhile ((flip notElem) "[]") list)

containsAbba :: String -> Bool
containsAbba (a:b:c:d:xs)
  | isAbba a b c d = True
  | otherwise = containsAbba (b:c:d:xs)
containsAbba _ = False

isAbba :: Char -> Char -> Char -> Char -> Bool
isAbba a b c d =
  a == d && b == c && a /= b
