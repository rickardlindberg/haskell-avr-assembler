module Parser.MakeDoc where

import qualified Data.Map as M
import Parser.Doc

emptyDoc :: Doc
emptyDoc = Doc [] [] []

addConstructor :: Constructor -> Doc -> Doc
addConstructor constructor (Doc a b c) = (Doc (constructor:a) b c)

addEncode :: EncodeCase -> Doc -> Doc
addEncode encode (Doc a b c) = (Doc a (encode:b) c)

makeConstructor :: Name -> Arguments -> Constructor
makeConstructor = Constructor

makeEncodeCase :: Name -> Arguments -> [[Char]] -> EncodeCase
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
