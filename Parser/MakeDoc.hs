module Parser.MakeDoc where

import qualified Data.Map as M

import Parser.Doc
import Parser.Word16Pattern

emptyDoc :: Doc
emptyDoc = Doc [] [] []

addConstructor :: Constructor -> Doc -> Doc
addConstructor x (Doc a b c) = (Doc (x:a) b c)

addEncode :: EncodeCase -> Doc -> Doc
addEncode x (Doc a b c) = (Doc a (x:b) c)

addDecode :: DecodeCase -> Doc -> Doc
addDecode x (Doc a b c) = (Doc a b (x:c))

makeConstructor :: Name -> Arguments -> Constructor
makeConstructor = Constructor

makeEncodeCase :: Name -> Arguments -> [Word16Pattern] -> EncodeCase
makeEncodeCase name arguments wordPatterns =
    EncodeCase name arguments (map makeWordSpec wordPatterns)

makeWordSpec :: Word16Pattern -> WordSpec
makeWordSpec (Word16Pattern bits) = WordSpec shiftOperations
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

makeDecodeCase :: Name -> Arguments -> [Word16Pattern] -> DecodeCase
makeDecodeCase name arguments wordPatterns =
    DecodeCase name arguments (createMask (head wordPatterns)) (createValue (head wordPatterns))
    where
        createMask :: Word16Pattern -> Int
        createMask = sumBits (`elem` "01")

        createValue :: Word16Pattern -> Int
        createValue = sumBits (`elem` "1")
