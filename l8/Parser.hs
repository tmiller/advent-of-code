module Parser (Instruction (..), Axis (..), parse)
where

import Data.List (isPrefixOf)
import Data.Char (isDigit)


data Axis
  = Column
  | Row
  deriving Show

data Instruction
  = Rect Int Int
  | Rotate Axis Int Int
  | NoOp
  deriving Show


parse :: String -> [Instruction]
parse = map parseInstruction . lines


parseInstruction :: String -> Instruction
parseInstruction raw
  | isPrefixOf "rect" raw          = (uncurry Rect) args
  | isPrefixOf "rotate column" raw = (uncurry $ Rotate Column) args
  | isPrefixOf "rotate row" raw    = (uncurry $ Rotate Row) args
  | otherwise                      = NoOp
  where
    args = parseNumericArgs raw


parseNumericArgs :: String -> (Int, Int)
parseNumericArgs raw =
  (read first, read second)
  where
    first = scanDigit raw
    second = scanDigit $ skipDigit raw


scanDigit :: String -> String
scanDigit = takeWhile isDigit . dropWhile (not . isDigit)


skipDigit :: String -> String
skipDigit = dropWhile isDigit . dropWhile (not . isDigit)
