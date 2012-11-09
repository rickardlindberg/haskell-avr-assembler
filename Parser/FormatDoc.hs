module Parser.FormatDoc where

import Data.List

import Parser.Doc

formatDoc :: Doc -> String
formatDoc (Doc constructors encodes decodes) = unlines
    [ "import Data.Word"
    , ""
    , "data Instruction ="
    , concat $ intersperse "\n    | " $ reverse $ map formatConstructor constructors
    , ""
    , "encode :: Instruction -> [Word16]"
    , concat $ intersperse "\n    " $ reverse $ map formatEncodeCase encodes
    , ""
    , "decode :: [Word16] -> (Instruction, [Word16])"
    , "decode = undefined"
    ]

formatConstructor :: Constructor -> String
formatConstructor (Constructor name args) =
    name ++ concat (replicate (length args) " Int")

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

