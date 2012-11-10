module Parser.Doc where

import qualified Data.Map as M

data Doc            = Doc [Constructor] [Arbitrary] [EncodeCase] [DecodeCase]

data Constructor    = Constructor Name Arguments

data Arbitrary      = Arbitrary Name Arguments (M.Map Argument Int)

data EncodeCase     = EncodeCase Name Arguments [WordSpec]
data WordSpec       = WordSpec [ShiftOperation]
data ShiftOperation = ShiftOperation BitValue Int
data BitValue       = Zero | One | Argument Argument Int

data DecodeCase     = DecodeCase Name Arguments Int Int Int [ExtractSpec]
data ExtractSpec    = ExtractSpec Char [(Int, Int, Int)]

type Name           = String
type Arguments      = [Argument]
type Argument       = Char
