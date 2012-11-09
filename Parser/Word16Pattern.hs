module Parser.Word16Pattern where

data Word16Pattern = Word16Pattern [Char]

makeWord16Pattern :: [Char] -> Word16Pattern
makeWord16Pattern pattern | length pattern == 16 = Word16Pattern pattern

sumBits :: (Char -> Bool) -> Word16Pattern -> Int
sumBits include (Word16Pattern bits) =
      sum
    $ map ((^) 2 . snd)
    $ filter (include . fst)
    $ zip bits [15, 14..]
