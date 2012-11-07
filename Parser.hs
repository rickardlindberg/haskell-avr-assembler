module Parser where

import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

import Parser.Doc
import Parser.MakeDoc

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

stateToString s = concat $ []
    ++ ["import Data.Word\n"]
    ++ ["\n"]
    ++ ["data Instruction =\n      "]
    ++ (intersperse "\n    | " $ reverse $ constructors s)
    ++ ["\n\n"]
    ++ ["encode :: Instruction -> [Word16]\n"]
    ++ ["encode = undefined\n"]
    ++ (reverse $ encodes s)
    ++ ["\n"]
    ++ ["decode :: [Word16] -> (Instruction, [Word16])\n"]
    ++ ["decode = undefined\n"]
--    ++ ["decode (word:words)"]
    ++ (reverse $ decodes s)
    ++ ["\n"]

formatConstructor :: String -> Int -> String
formatConstructor name argumentCount =
    name ++ concat (replicate argumentCount " Int")

formatEncodeCase :: EncodeCase -> String
formatEncodeCase c = concat
    [ "encode (" ++ name c ++ concatMap (\c -> [' ', c]) (arguments c) ++ ") =\n"
    , "    [\n"
    , intercalate "    ,\n" groups
    , "    ]\n"
    ]
    where
        groups = map formatWordSpec (wordSpecs c)

formatWordSpec (WordSpec operations) =
    "            " ++ intercalate "\n        .|. " (map formatShiftOperation operations) ++ "\n"

formatShiftOperation (ShiftOperation value position) =
    formatBitValue value ++ " `shiftL` " ++ show position

formatBitValue Zero           = "0                     "
formatBitValue One            = "1                     "
formatBitValue (Argument c i) = "((" ++ [c] ++ " `shiftR` " ++ show i ++ ") .&. 1)"

wordBits :: [Char] -> [[Char]]
wordBits bits | length bits == 16 = [bits]
              | length bits == 32 = [take 16 bits, drop 16  bits]

type Translator a = CharParser GeneratorState a

generateHaskell :: Translator String
generateHaskell = many instructionDescription >> fmap stateToString getState

instructionDescription :: Translator ()
instructionDescription = do
    name <- many1 letter
    spaces
    arguments <- argument `sepBy` (char ',')
    spaces
    bits <- (bit arguments) `sepBy` spaces
    newline
    addConstructor $ formatConstructor name (length arguments)
    addEncode $ formatEncodeCase $ makeEncodeCase name arguments $ wordBits bits
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
