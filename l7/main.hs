module Main
where

import System.IO
import Data.List (groupBy, sortBy)
import Data.Function
import Data.Ord

import qualified TLS
import qualified SSL
import qualified Packet

main :: IO ()
main =
  withFile "input.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let packets = lines contents
    putStrLn $ show $ length $ filter (TLS.validate . Packet.parsePacket) packets
    putStrLn $ show $ length $ filter (SSL.validate . Packet.parsePacket) packets
  )
