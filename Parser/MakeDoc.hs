module Parser.MakeDoc where

import qualified Data.Map as M

import Parser.Doc
import Parser.Word16Pattern

emptyDoc :: Doc
emptyDoc = Doc [] [] [] []

reverseDocParts :: Doc -> Doc
reverseDocParts (Doc a b c d) = Doc (reverse a) (reverse b)
                                    (reverse c) (reverse d)

addConstructor :: Constructor -> Doc -> Doc
addConstructor x (Doc a b c d) = (Doc (x:a) b c d)

addArbitrary :: Arbitrary -> Doc -> Doc
addArbitrary x (Doc a b c d) = (Doc a (x:b) c d)

addEncode :: EncodeCase -> Doc -> Doc
addEncode x (Doc a b c d) = (Doc a b (x:c) d)

addDecode :: DecodeCase -> Doc -> Doc
addDecode x (Doc a b c d) = (Doc a b c (x:d))

makeConstructor :: Name -> Arguments -> Constructor
makeConstructor = Constructor

makeArbitrary :: Name -> Arguments -> [Word16Pattern] -> Arbitrary
makeArbitrary name args patterns =
    Arbitrary name args (M.unionsWith (+) $ map countPieces patterns)

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
        createExtracts [p1]     = foo 0  0 p1
        createExtracts [p1, p2] = mergeExtractSpecs (foo 16 0 p1) (foo 0 1 p2)

        foo offset n p1 =
            M.elems $ M.mapWithKey (\k v -> ExtractSpec k (map (\(x, y) -> (n, x, y)) (zip v [(length v) - 1, (length v) - 2 ..]))) (M.filterWithKey (\k v -> k `notElem` "01") (positions offset p1))

        mergeExtractSpecs :: [ExtractSpec] -> [ExtractSpec] -> [ExtractSpec]
        mergeExtractSpecs []     res = res
        mergeExtractSpecs (x:xs) res = mergeExtractSpecs xs (mergeSingle x res)

        mergeSingle :: ExtractSpec -> [ExtractSpec] -> [ExtractSpec]
        mergeSingle x [] = [x]
        mergeSingle one@(ExtractSpec arg tuples) (two@(ExtractSpec arg' tuples'):xs)
            | arg == arg' = ExtractSpec arg (tuples ++ tuples') : xs
            | otherwise   = two : mergeSingle one xs
