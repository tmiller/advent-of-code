module Main
where

import System.IO

import qualified Parser
import qualified Board

main :: IO ()
main = 
  withFile "input.txt" ReadMode (\handle -> do
    -- contents <- hGetContents handle
    mapM_ putStrLn
      $ Board.showBoard
      -- $ Board.paintBoard Board.init 3 2
      -- $Board.process Board.init Parser.NoOp
      $Board.process Board.init $ Parser.Rect 3 2
  )

-- putStrLn $ show $ Parser.parse contents
