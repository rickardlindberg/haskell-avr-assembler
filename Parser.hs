module Parser where

import Text.ParserCombinators.Parsec

import qualified Parser.Doc as Doc
import Parser.MakeDoc
import Parser.FormatDoc

type Translator a = CharParser Doc.Doc a

main = do
    input <- getContents
    case runParser generateHaskell (Doc.Doc [] [] []) "" input of
        Left  err -> print err
        Right s   -> putStr s

generateHaskell :: Translator String
generateHaskell = many instructionDescription >> fmap formatDoc getState

instructionDescription :: Translator ()
instructionDescription = do
    name <- many1 letter
    spaces
    arguments <- argument `sepBy` (char ',')
    spaces
    bits <- (bit arguments) `sepBy` spaces
    newline
    updateState $ Doc.addConstructor $ Doc.Constructor name arguments
    updateState $ Doc.addEncode $ makeEncodeCase name arguments $ wordBits bits
    return ()
    where
        spaces = skipMany (char ' ')
        argument = lower
        bit fields = oneOf ('0':'1':fields)

wordBits :: [Char] -> [[Char]]
wordBits bits | length bits == 16 = [bits]
              | length bits == 32 = [take 16 bits, drop 16  bits]
