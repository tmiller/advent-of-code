module SSL (validate)
where

import Data.List (isInfixOf)
import qualified Packet


type Aba = String
type Bab = String


validate :: Packet.Packet -> Bool
validate packet =
  validateSupernet supernet && validateHypernet babs hypernet
  where
    babs = extractBabs supernet
    supernet = Packet.getSupernet packet
    hypernet = Packet.getHypernet packet

validateSupernet :: Packet.Sequence -> Bool
validateSupernet = or . map hasAba

validateHypernet :: [Bab] -> Packet.Sequence -> Bool
validateHypernet babs = or . map (hasBab babs)

extractBabs :: Packet.Sequence -> [Bab]
extractBabs = map abaToBab . concat . map getAbas

abaToBab :: Aba -> Bab
abaToBab (a:b:c:[]) = (b:a:b:[])
abaToBab _ = []

hasBab :: [Bab] -> String -> Bool
hasBab babs hypernet =
  or $ map ((flip isInfixOf) hypernet) babs

hasAba :: String -> Bool
hasAba =
  not . null . getAbas

getAbas :: String -> [Aba]
getAbas (a:b:c:xs)
  | isAba a b c = (a:b:c:[]) : getAbas (b:c:xs)
  | otherwise = getAbas (b:c:xs)
getAbas _ = []

isAba :: Char -> Char -> Char -> Bool
isAba a b c =
  a == c && a /= b
