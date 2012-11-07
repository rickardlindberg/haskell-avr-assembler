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

data EncodeCase = EncodeCase
    { name      :: String
    , arguments :: [Char]
    , wordSpecs :: [WordSpec]
    }

data WordSpec = WordSpec [ShiftOperation]

data ShiftOperation = ShiftOperation
    { value    :: BitValue
    , position :: Int
    }

data BitValue = Zero | One | Argument Char Int

makeEncodeCase :: String -> [Char] -> [[Char]] -> EncodeCase
makeEncodeCase name arguments wordBits = EncodeCase name arguments wordSpecs
    where
      wordSpecs = map makeWordSpec wordBits

makeWordSpec :: [Char] -> WordSpec
makeWordSpec bits = WordSpec shiftOperations
    where
      sources         = makeSources bits (initialLeft bits M.empty)
      bitValues       = zipWith makeBitValue bits sources
      destinations    = [15, 14..]
      shiftOperations = zipWith ShiftOperation bitValues destinations

      makeSources :: [Char] -> M.Map Char Int -> [Int]
      makeSources []     _    = []
      makeSources (b:bs) left =
        let value = (left M.! b) - 1
            rest  = makeSources bs (M.adjust (subtract 1) b left)
        in  value:rest

      initialLeft :: [Char] -> M.Map Char Int -> M.Map Char Int
      initialLeft []     left = left
      initialLeft (b:bs) left = initialLeft bs $ M.insertWith (+) b 1 left

      makeBitValue :: Char -> Int -> BitValue
      makeBitValue '0' _    = Zero
      makeBitValue '1' _    = One
      makeBitValue b source = Argument b source

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
