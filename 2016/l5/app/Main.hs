module Main where

import Lib
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.MD5
import Data.List (isPrefixOf, length, reverse, sortBy)
import Data.Ord (comparing)

type CodePoint = (Char, Char)

main :: IO ()
main = do
  putStrLn $ show $ applyAglorithm "cxdnnyjw" leadingZeros getSixth reverse
  putStrLn $ show $ applyAglorithm "cxdnnyjw" validCode getCodePoint decodeCodePoints
  
applyAglorithm :: String -> ([a] -> String -> Bool) -> (String -> a) -> ([a] -> String) -> String
applyAglorithm code predicate getKey sortKey =
  applyAglorithm' code [] [0..]
  where
    applyAglorithm' code result (x:xs)
      | (length result) == 8 = (sortKey result)
      | predicate result hashed = applyAglorithm' code (key:result) xs
      | otherwise = applyAglorithm' code result xs
      where
        key = (getKey hashed)
        hashed = Main.hash code x

leadingZeros :: String -> String -> Bool
leadingZeros _ = isPrefixOf (replicate 5 '0')

getSixth :: [a] -> a
getSixth = head . drop 5

getSeventh :: [a] -> a
getSeventh = head . drop 6

validCode :: [CodePoint] -> String -> Bool
validCode results hashed =
  (leadingZeros "" hashed)
    && (notElem index (map fst results))
    && (elem index ['0'..'7'])
  where
    index = getSixth hashed

getCodePoint :: String -> CodePoint
getCodePoint hashed =
  (getSixth hashed, getSeventh hashed)

decodeCodePoints :: [CodePoint] -> String
decodeCodePoints =
  map snd . sortBy (comparing fst)

hash :: String -> Integer -> String
hash seed num =
  show $ md5 $ C.pack $ seed ++ (show num)
