module Parser.Doc where

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

