-- | Binary processing extensions to Attoparsec.
module Data.Attoparsec.Binary
       (
         anyWordNbe
       , anyWordNle
       , anyWord16be
       , anyWord16le
       , anyWord32be
       , anyWord32le
       , anyWord64be
       , anyWord64le
       , wordNbe
       , wordNle
       , word16be
       , word16le
       , word32be
       , word32le
       , word64be
       , word64le
       ) where

import Data.Attoparsec.ByteString
import Data.Bits
import qualified Data.ByteString as B
import Data.Word

byteSize :: (FiniteBits a) => a -> Int
byteSize = (`div` 8) . finiteBitSize

pack :: (Bits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

anyWordN :: (FiniteBits a) => (B.ByteString -> a) -> Parser a
anyWordN = anyWordN' undefined
  where anyWordN' :: (FiniteBits a) => a -> (B.ByteString -> a) -> Parser a
        anyWordN' = flip fmap . Data.Attoparsec.ByteString.take . byteSize

-- | Match any big-endian word. It is recommended that the number of
-- bits is divisible by 8, otherwise the N `mod` 8 least significant bits will
-- be ignored and not consumed.
anyWordNbe :: (FiniteBits a, Num a) => Parser a
anyWordNbe = anyWordN pack

-- | Match any little-endian word. It is recommended that the number of
-- bits is divisible by 8, otherwise the N `mod` 8 most significant bits will
-- be ignored and not consumed.
anyWordNle :: (FiniteBits a, Num a) => Parser a
anyWordNle = anyWordN $ pack . B.reverse

-- | Match any 16-bit big-endian word.
anyWord16be :: Parser Word16
anyWord16be = anyWordNbe

-- | Match any 16-bit little-endian word.
anyWord16le :: Parser Word16
anyWord16le = anyWordNle

-- | Match any 32-bit big-endian word.
anyWord32be :: Parser Word32
anyWord32be = anyWordNbe

-- | Match any 32-bit little-endian word.
anyWord32le :: Parser Word32
anyWord32le = anyWordNle

-- | Match any 64-bit big-endian word.
anyWord64be :: Parser Word64
anyWord64be = anyWordNbe

-- | Match any 64-bit little-endian word.
anyWord64le :: Parser Word64
anyWord64le = anyWordNle

unpack :: (FiniteBits a, Integral a) => a -> B.ByteString
unpack x = B.pack $ map takeByte [0, 8 .. finiteBitSize x - 1]
  where takeByte :: (Num a) => Int -> a
        takeByte = fromIntegral . shiftR x

wordN :: (a -> B.ByteString) -> a -> Parser a
wordN u w = string (u w) >> return w

-- | Match a specific big-endian word. It is recommended that the number of
-- bits is divisible by 8, otherwise the N `mod` 8 most significant bits will
-- be ignored and not consumed.
wordNbe :: (FiniteBits a, Integral a) => a -> Parser a
wordNbe = wordN $ B.reverse . unpack

-- | Match a specific little-endian word. It is recommended that the number of
-- bits is divisible by 8, otherwise the N `mod` 8 least significant bits will
-- be ignored and not consumed.
wordNle :: (FiniteBits a, Integral a) => a -> Parser a
wordNle = wordN unpack

-- | Match a specific 16-bit big-endian word.
word16be :: Word16 -> Parser Word16
word16be = wordNbe

-- | Match a specific 16-bit little-endian word.
word16le :: Word16 -> Parser Word16
word16le = wordNle

-- | Match a specific 32-bit big-endian word.
word32be :: Word32 -> Parser Word32
word32be = wordNbe

-- | Match a specific 32-bit little-endian word.
word32le :: Word32 -> Parser Word32
word32le = wordNle

-- | Match a specific 64-bit big-endian word.
word64be :: Word64 -> Parser Word64
word64be = wordNbe

-- | Match a specific 64-bit little-endian word.
word64le :: Word64 -> Parser Word64
word64le = wordNle
