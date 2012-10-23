module Avr.Parser where

import Data.List
import qualified Data.Map as M
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

formatEncode :: String -> [Char] -> [[Char]] -> String
formatEncode name arguments wordBits = lhs ++ rhs
    where
      lhs = "encode (" ++ name ++ " " ++ (intersperse ' ' arguments) ++ ") =\n"
      rhs = "    [\n" ++ intercalate "    ,\n" (map wordLines wordBits) ++ "    ]\n"
      wordLines bits =
        "            " ++ intercalate "        .|. " (map (++ "\n") (subWordLines bits))
      subWordLines bits = loop bits [15,14..] $ initialLeft bits M.empty
      loop [] _ _ = []
      loop ('0':bs) (p:ps) left =
        ("0                      `shiftL` " ++ show p) : loop bs ps left
      loop ('1':bs) (p:ps) left =
        ("1                      `shiftL` " ++ show p) : loop bs ps left
      loop (b:bs)   (p:ps) left =
        ("((" ++ [b] ++ " `shiftR` " ++ show ((left M.! b) - 1)
         ++ ") .&. 1) `shiftL` " ++ show p) :
        (loop bs ps $ M.adjust (subtract 1) b left)
      initialLeft [] left = left
      initialLeft (b:bs) left = initialLeft bs $ M.insertWith (+) b 1 left

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
    addEncode $ formatEncode name arguments $ wordBits bits
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
