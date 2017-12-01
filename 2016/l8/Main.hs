module Main
where

import System.IO
import Data.List (transpose, replicate)

import qualified Parser
import qualified Board

main :: IO ()
main =
  withFile "input.txt" ReadMode (\handle -> do
    contents <- hGetContents handle
    let instructions = Parser.parse contents
    let boards = scanl Board.process Board.init instructions
    let board = foldl Board.process Board.init instructions
    printBoards boards
    putStrLn $ show $ length $ filter id $ concat board
  )


printBoards board =  do
  mapM_ printBoard board


printBoard board = do
  mapM_ putStrLn $ Board.showBoard board
  putStrLn (replicate 50 '-')
