module Main
where


import Control.Applicative
import Control.Monad.Identity (Identity)
import System.Environment (getArgs)
import System.IO

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>), parse)


main :: IO ()
main = do
    args <- getArgs
    result <- parseFile parser (last args)
    case result of
        Left err -> print err
        Right xs -> print $ length $ concat xs

parseFile rule fname = do
    input <- readFile fname
    return (parse rule fname input)


parser = Parsec.many (contents <|> instruction)

contents :: Parsec.Parsec String () String
contents =
    Parsec.many1 $ Parsec.noneOf "(\n"

instruction :: Parsec.Parsec String () String
instruction = do
    Parsec.char '('
    nChars <- Parsec.many1 Parsec.digit
    Parsec.char 'x'
    nTimes <- Parsec.many1 Parsec.digit
    Parsec.char ')'
    strRepeat <- Parsec.count (read nChars) Parsec.anyChar
    let innerText = (concat $ replicate (read nTimes) strRepeat)
    return (nestedParse innerText)

nestedParse text =
    case parse parser "(nested instructions)" text of
        Left err -> ""
        Right result -> concat result
