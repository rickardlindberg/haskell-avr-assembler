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
makeWordSpec wordPattern@(Word16Pattern bits) = WordSpec shiftOperations
    where
      sources         = makeSources bits (countPieces wordPattern)
      bitValues       = zipWith makeBitValue bits sources
      destinations    = [15, 14..]
      shiftOperations = zipWith ShiftOperation bitValues destinations

      makeSources :: [Char] -> M.Map Char Int -> [Int]
      makeSources []     _    = []
      makeSources (b:bs) left =
        let value = (left M.! b) - 1
            rest  = makeSources bs (M.adjust (subtract 1) b left)
        in  value:rest

      makeBitValue :: Char -> Int -> BitValue
      makeBitValue '0' _    = Zero
      makeBitValue '1' _    = One
      makeBitValue b source = Argument b source

makeDecodeCase :: Name -> Arguments -> [Word16Pattern] -> DecodeCase
makeDecodeCase name arguments wordPatterns =
    DecodeCase name
               arguments
               (createMask (head wordPatterns))
               (createValue (head wordPatterns))
               (length wordPatterns)
               (createExtracts wordPatterns)
    where
        createMask :: Word16Pattern -> Int
        createMask = sumBits (`elem` "01")

        createValue :: Word16Pattern -> Int
        createValue = sumBits (`elem` "1")

        createExtracts :: [Word16Pattern] -> [ExtractSpec]
        createExtracts [p1]     = foo 0 p1
        createExtracts [p1, p2] = foo 0 p1 ++ foo 1 p2

        foo n p1 =
            M.elems $ M.mapWithKey (\k v -> ExtractSpec k (map (\(x, y) -> (n, x, y)) (zip v [(length v) - 1, (length v) - 2 ..]))) (M.filterWithKey (\k v -> k `notElem` "01") (positions p1))
