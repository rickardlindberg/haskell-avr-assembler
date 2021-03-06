module Parser.FormatDoc where

import Data.List
import qualified Data.Map as M

import Parser.Doc

formatDoc :: Doc -> String
formatDoc (Doc constructors arbitraries encodes decodes) = unlines
    [ "module Instructions where"
    , ""
    , "import Control.Monad"
    , "import Data.Bits"
    , "import Data.Word"
    , "import Test.QuickCheck (Arbitrary(), arbitrary, oneof, choose, Gen)"
    , ""
    , "data Instruction ="
    , "      " ++ (intercalate "\n    | " $ map formatConstructor constructors)
    , "    deriving (Show, Eq)"
    , ""
    , "instance Arbitrary Instruction where"
    , "    arbitrary = oneof"
    , "        [ " ++ (intercalate "\n        , " $ map formatArbitrary arbitraries)
    , "        ]"
    , "        where"
    , "            numberWithByteSize :: Int -> Gen Word16"
    , "            numberWithByteSize byteSize = liftM fromIntegral (choose (0, maxForByteSize byteSize))"
    , "            maxForByteSize :: Int -> Int"
    , "            maxForByteSize byteSize = 2^byteSize - 1"
    , ""
    , "encode :: Instruction -> [Word16]"
    , intercalate "    \n" $ map formatEncodeCase encodes
    , ""
    , "decode :: [Word16] -> (Instruction, [Word16])"
    , "decode words"
    , prepend "    | " $ map formatDecodeCase decodes
    ]

formatConstructor :: Constructor -> String
formatConstructor (Constructor name args) =
    name ++ concat (replicate (length args) " Word16")

formatArbitrary :: Arbitrary -> String
formatArbitrary (Arbitrary name args pieces) =
    liftVersion args ++ " " ++ name ++ prepend " " innerArbitraries
    where
        liftVersion args | length args == 0 = "return"
                         | length args == 1 = "liftM "
                         | otherwise        = "liftM" ++ show (length args)
        innerArbitraries = map innerArbitrary args
        innerArbitrary arg = "(numberWithByteSize " ++ show (pieces M.! arg) ++ ")"

formatEncodeCase :: EncodeCase -> String
formatEncodeCase (EncodeCase name args wordSpecs) = concat
    [ "encode (" ++ formatData name args ++ ") =\n"
    , "    [\n"
    , intercalate "    ,\n" groups
    , "    ]"
    ]
    where
        groups = map formatWordSpec wordSpecs

formatWordSpec (WordSpec operations) =
    "            " ++ intercalate "\n        .|. " (map formatShiftOperation operations) ++ "\n"

formatShiftOperation (ShiftOperation value position) =
    formatBitValue value ++ " `shiftL` " ++ show position

formatBitValue Zero           = "0                     "
formatBitValue One            = "1                     "
formatBitValue (Argument c i) = "((" ++ [c] ++ " `shiftR` " ++ show i ++ ") .&. 1)"

formatDecodeCase :: DecodeCase -> String
formatDecodeCase (DecodeCase name args mask value numWords extracts) = unlines
    [ "head words .&. " ++ show mask ++ " == " ++ show value ++ " ="
    , "        let"
    , "            " ++ (intercalate "\n            " $ map (\n -> "word" ++ show n ++ " = words !! " ++ show n) [0..numWords-1])
    , "            newwords = drop " ++ show numWords ++ " words"
    , intercalate "\n" $ map formatExtractSpec extracts
    , "        in (" ++ formatData name args ++ ", newwords)"
    ]

formatExtractSpec :: ExtractSpec -> String
formatExtractSpec (ExtractSpec arg extracts) =
    "            " ++ [arg] ++ " =     " ++
    intercalate "\n                .|. "
    ((map (\(word, src, dest) -> "(((word" ++ show word ++ " `shiftR` " ++ show src ++ ") .&. 1) `shiftL` " ++ show dest ++ ")") extracts))

formatData :: Name -> Arguments -> String
formatData name args = name ++ concatMap (\c -> [' ', c]) args

prepend :: String -> [String] -> String
prepend x xs = concatMap (x ++) xs
