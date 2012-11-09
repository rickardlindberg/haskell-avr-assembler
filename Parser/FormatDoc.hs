module Parser.FormatDoc where

import Data.List

import Parser.Doc

formatDoc :: Doc -> String
formatDoc (Doc constructors encodes decodes) = unlines
    [ "import Data.Word"
    , ""
    , "data Instruction ="
    , "      " ++ (concat $ intersperse "\n    | " $ reverse $ map formatConstructor constructors)
    , ""
    , "encode :: Instruction -> [Word16]"
    , concat $ intersperse "    \n" $ reverse $ map formatEncodeCase encodes
    , ""
    , "decode :: [Word16] -> (Instruction, [Word16])"
    , "decode words ="
    , "    | " ++ (concat $ intersperse "    | " $ reverse $ map formatDecodeCase decodes)
    ]

formatConstructor :: Constructor -> String
formatConstructor (Constructor name args) =
    name ++ concat (replicate (length args) " Int")

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
    [ "word .&. " ++ show mask ++ " == " ++ show value ++ " ="
    , "        let"
    , "            " ++ (intercalate "\n            " $ map (\n -> "word" ++ show n ++ " = words !! " ++ show n) [0..numWords-1])
    , "            words = drop " ++ show numWords ++ " words"
    , intercalate "\n" $ map formatExtractSpec extracts
    , "        in (" ++ formatData name args ++ ", words)"
    ]

formatExtractSpec :: ExtractSpec -> String
formatExtractSpec (ExtractSpec arg extracts) =
    "            " ++ [arg] ++ " =     " ++
    intercalate "\n                .|. "
    ((map (\(word, src, dest) -> "(((word" ++ show word ++ " `shiftR` " ++ show src ++ ") .&. 1) `shiftL` " ++ show dest ++ ")") extracts))

formatData :: Name -> Arguments -> String
formatData name args = name ++ concatMap (\c -> [' ', c]) args
