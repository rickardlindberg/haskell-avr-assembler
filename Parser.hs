module Parser where

import System.Exit
import System.IO
import Text.ParserCombinators.Parsec

import Parser.Doc
import Parser.FormatDoc
import Parser.MakeDoc
import Parser.Word16Pattern

type Translator a = CharParser Doc a

main = do
    input <- getContents
    case runParser generateHaskell emptyDoc "" input of
        Left  err -> hPrint stderr err >> exitFailure
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
    updateState $ addConstructor $ makeConstructor name arguments
    updateState $ addArbitrary   $ makeArbitrary name arguments $ wordPatterns bits
    updateState $ addEncode      $ makeEncodeCase name arguments $ wordPatterns bits
    updateState $ addDecode      $ makeDecodeCase name arguments $ wordPatterns bits
    where
        spaces = skipMany (char ' ')
        argument = lower
        bit fields = oneOf ('0':'1':fields)

wordPatterns :: [Char] -> [Word16Pattern]
wordPatterns bits | length bits == 16 = [ makeWord16Pattern bits ]
                  | length bits == 32 = [ makeWord16Pattern $ take 16 bits
                                        , makeWord16Pattern $ drop 16  bits
                                        ]
