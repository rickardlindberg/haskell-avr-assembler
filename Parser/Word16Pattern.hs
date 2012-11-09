module Parser.Word16Pattern where

import qualified Data.Map as M

data Word16Pattern = Word16Pattern [Char]

allPositions :: [Int]
allPositions = [15, 14..]

makeWord16Pattern :: [Char] -> Word16Pattern
makeWord16Pattern pattern | length pattern == 16 = Word16Pattern pattern

sumBits :: (Char -> Bool) -> Word16Pattern -> Int
sumBits include (Word16Pattern bits) =
      sum
    $ map ((^) 2 . snd)
    $ filter (include . fst)
    $ zip bits allPositions

countPieces :: Word16Pattern -> M.Map Char Int
countPieces (Word16Pattern bits) = initialLeft bits M.empty
    where
        initialLeft :: [Char] -> M.Map Char Int -> M.Map Char Int
        initialLeft []     left = left
        initialLeft (b:bs) left = initialLeft bs $ M.insertWith (+) b 1 left
