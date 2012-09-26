module Avr.Parser where

import Data.List
import Text.ParserCombinators.Parsec

data GeneratorState = GeneratorState
    { constructors :: [String]
    , encodes      :: [String]
    , decodes      :: [String]
    }

addConstructor constructor = updateState $
    \(s) -> s { constructors = constructor : constructors s }

addEncode encode = updateState $
    \(s) -> s { encodes = encode : encodes s }

addDecode decode = updateState $
    \(s) -> s { decodes = decode : decodes s }

stateToString s = concatMap (++ "\n") $ []
    ++ ["data Instruction ="]
    ++ (intersperse "|" $ reverse $ constructors s)
    ++ ["encode :: Instruction -> [Word16]"]
    ++ (reverse $ encodes s)
    ++ ["decode :: [Word16] -> (Instruction, [Word16])"]
    ++ ["decode (word:words)"]
    ++ (reverse $ decodes s)

type Translator a = CharParser GeneratorState a

generateHaskell :: Translator String
generateHaskell = many instructionDescription >> fmap stateToString getState

instructionDescription :: Translator ()
instructionDescription = do
    name <- many1 upper
    spaces
    arguments <- argument `sepBy` (char ',')
    spaces
    bits <- (bit arguments) `sepBy` spaces
    newline
    return ()
    where
        spaces = skipMany (char ' ')
        argument = lower
        bit fields = oneOf ('0':'1':fields)

main = do
    input <- getContents
    case runParser generateHaskell (GeneratorState [] [] []) "" input of
        Left  err -> print err
        Right s   -> putStr s
