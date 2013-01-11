module Parser where

import Control.Monad
import Data.Word
import Data.Bits
import Data.Char
import Text.ParserCombinators.Parsec

type Operation = [Word8]

numbers :: Parser Char
numbers = oneOf ['0'..'9']

hexNumbers :: Parser Char
hexNumbers = (try numbers) <|> oneOf (['A'..'F'] ++ ['a'..'f'])

parseOp :: Parser Operation
parseOp =
    foldr1 (\a b -> (try a) <|> b) operations

operations :: [Parser Operation]
operations =
    [ word
    , add
    , jumpRegister
    ]

octets :: Word32 -> [Word8]
octets w = 
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

pairs :: Word16 -> [Word8]
pairs w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

nOf :: Int -> Parser Char -> Parser String
nOf 0 chrs = return ""
nOf n chrs = do
    chr <- chrs
    rest <- nOf (n-1) chrs
    return (chr:rest)

binaryNumber :: Parser Word32
binaryNumber = do
    char '0'
    char 'b'
    binaryString <- nOf 32 (char '0' <|> char '1')
    return (binToW32 (reverse binaryString)) -- reverse so number is big endian
  where
    binToW32 :: String -> Word32
    binToW32 [] = 0
    binToW32 (x:xs) =
        if x == '1'
            then 1 + 2 * (binToW32 xs)
            else 2 * (binToW32 xs)

hexNumber :: Parser Word32
hexNumber = do
    char '0'
    char 'x'
    hexString <- nOf 8 (hexNumbers)
    return (hexToW32 (reverse hexString))
  where
    hexToW32 :: String -> Word32
    hexToW32 [] = 0
    hexToW32 (x:xs)
        | x `elem` ['0'..'9'] = (ox - 48) + (16 * hexToW32 xs)
        | x `elem` ['a'..'f'] = (ox - 87) + (16 * hexToW32 xs)
        | x `elem` ['A'..'F'] = (ox - 55) + (16 * hexToW32 xs)
      where
        ox = fromIntegral (ord x)

operation :: String -> Parser Operation -> Parser Operation
operation s p = do
    skipMany space
    string s
    skipMany1 space
    p

word :: Parser Operation
word = operation ".word" $ do
    number <- (try binaryNumber)
          <|> (try hexNumber)
    return (octets number)

registerEncoding :: Int -> Int -> Int -> Int -> Int -> Int -> Operation
registerEncoding op s t dest shift func =
    [ fromIntegral ((op `shiftL` 2) + (s `shiftR` 3))
    , fromIntegral ((s `shiftL` 5) + t)
    , fromIntegral ((dest `shiftL` 3) + (shift `shiftR` 2))
    , fromIntegral ((shift `shiftL` 6) + func)
    ]

jumpEncoding :: Int -> Int -> Operation
jumpEncoding s op =
    [ fromIntegral (s `shiftR` 3)
    , fromIntegral (s `shiftL` 5)
    , 0
    , fromIntegral op
    ]

register :: Parser Int
register = do
    char '$'
    number <- many1 (oneOf ['0'..'9'])
    let numInt = read number
    if numInt > 31 || numInt < 0
        then error "Not a valid register"
        else return numInt

argumentSeperator :: Parser ()
argumentSeperator = do
    skipMany space
    char ','
    skipMany space

add :: Parser Operation
add = operation "add" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t d 0 32

jumpRegister :: Parser Operation
jumpRegister = operation "jr" $ do
    s <- register
    return $ jumpEncoding s 8
