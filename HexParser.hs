module Avr.HexParser where

import Control.Monad
import Data.Char
import Data.Word
import Text.ParserCombinators.Parsec

type Address = Word16

main :: IO ()
main = do
    input <- getContents
    case parse hexParser "" input of
        Left  error -> print error
        Right res   -> print res

hexParser :: Parser [Word8]
hexParser = do
    bytes <- hexDataBytes
    hexEnd
    eof
    when (not (inSequence (map fst bytes)))
        (fail $ "memory corrupt: " ++ show (map fst bytes))
    return $ map snd bytes
    where
        inSequence :: [Address] -> Bool
        inSequence []        = True
        inSequence [x]       = True
        inSequence (x:y:res) = x + 1 == y && inSequence (y:res)

hexDataBytes :: Parser [(Address, Word8)]
hexDataBytes = fmap concat (many hexDataBytesFromLine)

hexDataBytesFromLine :: Parser [(Address, Word8)]
hexDataBytesFromLine = try $ do
    (record, bytes) <- hexLine
    when (record /= 0) $ fail "not data record"
    return bytes

hexEnd :: Parser ()
hexEnd = try $ do
    (record, bytes) <- hexLine
    when (record /= 1)      $ fail "not end record"
    when (not (null bytes)) $ fail "end should have no data"

hexLine :: Parser (Word8, [(Address, Word8)])
hexLine = do
    char ':'
    byteCount <- hexByte
    address   <- hexByte >>= \h -> hexByte >>= \l -> return (bigEndian h l)
    record    <- hexByte
    data_     <- count (fromIntegral byteCount) hexByte
    checksum  <- hexByte
    universalNewline
    -- TODO: verify checksum
    return $ (record, map (\(offset, b) -> (address+offset, b)) $ zip [0..] data_)

hexByte :: Parser Word8
hexByte = do
    [first, second]  <- fmap (map digitToInt) (count 2 hexDigit)
    return $ fromIntegral $ 16 * first + second

bigEndian :: Word8 -> Word8 -> Word16
bigEndian h l = 256 * fromIntegral h + fromIntegral l

universalNewline :: Parser ()
universalNewline = (try (string "\r\n") <|> string "\n") >> return ()
