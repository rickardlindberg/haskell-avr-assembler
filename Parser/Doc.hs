module Parser.Doc where

data Doc            = Doc [Constructor] [EncodeCase] [DecodeCase]

data Constructor    = Constructor Name Arguments

data EncodeCase     = EncodeCase Name Arguments [WordSpec]
data WordSpec       = WordSpec [ShiftOperation]
data ShiftOperation = ShiftOperation BitValue Int
data BitValue       = Zero | One | Argument Argument Int

data DecodeCase     = DecodeCase

type Name           = String
type Arguments      = [Argument]
type Argument       = Char
