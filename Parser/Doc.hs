module Parser.Doc where

data Doc = Doc
    { constructors :: [Constructor]
    , encodes      :: [EncodeCase]
    , decodes      :: [DecodeCase]
    }

addConstructor :: Constructor -> Doc -> Doc
addConstructor constructor (Doc a b c) = (Doc (constructor:a) b c)

addEncode :: EncodeCase -> Doc -> Doc
addEncode encode (Doc a b c) = (Doc a (encode:b) c)

data Constructor = Constructor
    { cname      :: String
    , carguments :: [Char]
    }

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

data DecodeCase = DecodeCase
    {
    }
