module Board (Board, Board.init, process, showBoard, paintCell, paintBoard)
where

import Parser (Instruction (..), Axis)

type Board = [[Bool]]

init :: Board
init =
  replicate 6 (replicate 50 False)

showBoard :: Board -> [String]
showBoard = map (map showBool)

showBool :: Bool -> Char
showBool (True)  = 'X'
showBool (False) = '.'


process :: Board -> Instruction -> Board
process board (Rect x y) = paintBoard board x y
process board _ = board

paintBoard :: Board -> Int -> Int -> Board
paintBoard board x y =
  foldl paintCell board cells
  where
    cells = concat $ map (\y -> map (\x -> (x,y)) [0..(x-1)]) [0..(y-1)]

paintCell :: Board -> (Int, Int) -> Board
paintCell board (x,y) =
  replaceNth y (replaceNth x True (board !! y)) board
  
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
  | n == 0    = (newVal:xs)
  | otherwise = x:replaceNth (n-1) newVal xs


shift :: Int -> [a] -> [a]
shift n xs = take (length xs) $ drop (negate n `mod` length xs) $ cycle xs

