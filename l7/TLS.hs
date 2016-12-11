module TLS (validate)
where

import qualified Packet

validate :: Packet.Packet -> Bool
validate packet =
  validateSupernet supernet && validateHypernet hypernet
  where
    supernet = Packet.getSupernet packet
    hypernet = Packet.getHypernet packet

validateSupernet :: Packet.Sequence -> Bool
validateSupernet = or . map hasAbba

validateHypernet :: Packet.Sequence -> Bool
validateHypernet = and . map hasNoAbba 

hasNoAbba :: String -> Bool
hasNoAbba = not . hasAbba

hasAbba :: String -> Bool
hasAbba (a:b:c:d:xs)
  | isAbba a b c d = True
  | otherwise = hasAbba (b:c:d:xs)
hasAbba _ = False

isAbba :: Char -> Char -> Char -> Char -> Bool
isAbba a b c d =
  a == d && b == c && a /= b
