module Parser
( parseOp
) where

import Control.Monad
import Data.Word
import Data.Bits
import Data.Char
import Text.ParserCombinators.Parsec

-- Defines an operation for the MIPS processor in binary
type Operation = [Word8]

-- parses a single decimal digit
numbers :: Parser Char
numbers = oneOf ['0'..'9']

-- parses a single hex digit
hexNumbers :: Parser Char
hexNumbers = (try numbers) <|> oneOf (['A'..'F'] ++ ['a'..'f'])

-- parses an operation using the list of operations parsers
parseOp :: Parser Operation
parseOp =
    foldr1 (\a b -> (try a) <|> b) operations

-- The list of supported operation parsers
operations :: [Parser Operation]
operations =
    [ word
    , add
    , Parser.subtract
    , multiply
    , multiplyUnsigned
    , divide
    , divideUnsigned
    , moveFromHigh
    , moveFromLow
    , loadImmediateAndSkip
    , loadWord
    , storeWord
    , branchOnEqual
    , branchOnNotEqual
    , jumpRegister
    , jumpAndLinkRegister
    ]

-- Splits a 32-bit integer into 4 8-bit integers
octets :: Word32 -> [Word8]
octets w = 
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

-- Splits a 16-bit integer into 2 8-bit integers
pairs :: Word16 -> [Word8]
pairs w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

-- Performs a parser n times, putting the results into a list
nOf :: Int -> Parser a -> Parser [a]
nOf 0 chrs = return []
nOf n chrs = do
    chr <- chrs
    rest <- nOf (n-1) chrs
    return (chr:rest)

-- Parses a 32-bit number in binary
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

-- Parses a 32-bit number in hexidecimal
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

-- Parses an operation with a given name and parser
-- Simplifies operations of the form 'name somethingElse'
operation :: String -> Parser Operation -> Parser Operation
operation s p = do
    skipMany space
    string s
    skipMany1 space
    p

-- Parses a '.word' command
word :: Parser Operation
word = operation ".word" $ do
    number <- (try binaryNumber)
          <|> (try hexNumber)
    return (octets number)

-- Uses the register command encoding to generate an operation
registerEncoding :: Int -> Int -> Int -> Int -> Int -> Int -> Operation
registerEncoding op s t dest shift func =
    [ fromIntegral ((op `shiftL` 2) + (s `shiftR` 3))
    , fromIntegral ((s `shiftL` 5) + t)
    , fromIntegral ((dest `shiftL` 3) + (shift `shiftR` 2))
    , fromIntegral ((shift `shiftL` 6) + func)
    ]

-- Uses the immediate encoding to generate an operation
immediateEncoding :: Int -> Int -> Int -> Int -> Operation
immediateEncoding op s t i =
    [ fromIntegral ((op `shiftL` 2) + (s `shiftR` 3))
    , fromIntegral ((s `shiftL` 5) + t)
    , iHigh
    , iLow
    ]
  where
    iParts = pairs (fromIntegral i)
    iHigh = iParts !! 0
    iLow = iParts !! 1

-- Uses the register command encoding to generate an operation
jumpEncoding :: Int -> Int -> Operation
jumpEncoding s op =
    [ fromIntegral (s `shiftR` 3)
    , fromIntegral (s `shiftL` 5)
    , 0
    , fromIntegral op
    ]

-- Parses a register argument
register :: Parser Int
register = do
    char '$'
    number <- many1 (oneOf ['0'..'9'])
    let numInt = read number
    if numInt > 31 || numInt < 0
        then error "Not a valid register"
        else return numInt

immediateNumber :: Parser Int
immediateNumber = do
    number <- many1 (oneOf ['0'..'9'])
    let numInt = read number
    if numInt > 65535 || numInt < 0
        then error "Not a valid 16-bit number"
        else return numInt

-- Parses a number and register of the form i($r) and returns (i, r)
bracketRegister :: Parser (Int, Int)
bracketRegister = do
    number <- immediateNumber
    char '('
    reg <- register
    char ')'
    return (number, reg)

-- Skips the spaces and seperator between arguments
argumentSeperator :: Parser ()
argumentSeperator = do
    skipMany space
    char ','
    skipMany space

-- Parses the 'add' command
add :: Parser Operation
add = operation "add" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t d 0 32

-- Parses the 'subtract command'
subtract :: Parser Operation
subtract = operation "sub" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t d 0 34

multiply :: Parser Operation
multiply = operation "mult" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t 0 0 24

multiplyUnsigned :: Parser Operation
multiplyUnsigned = operation "multu" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t 0 0 25

divide :: Parser Operation
divide = operation "div" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t 0 0 26

divideUnsigned :: Parser Operation
divideUnsigned = operation "divu" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t 0 0 27

moveFromHigh :: Parser Operation
moveFromHigh = operation "mfhi" $ do
    d <- register
    return $ registerEncoding 0 0 0 d 0 16

moveFromLow :: Parser Operation
moveFromLow = operation "mflo" $ do
    d <- register
    return $ registerEncoding 0 0 0 d 0 18

loadImmediateAndSkip :: Parser Operation
loadImmediateAndSkip = operation "lis" $ do
    d <- register
    return $ registerEncoding 0 0 0 d 0 20

loadWord :: Parser Operation
loadWord = operation "lw" $ do
    t <- register
    argumentSeperator
    (i, s) <- bracketRegister
    return $ immediateEncoding 35 s t i

storeWord :: Parser Operation
storeWord = operation "sw" $ do
    t <- register
    argumentSeperator
    (i, s) <- bracketRegister
    return $ immediateEncoding 43 s t i

setLessThan :: Parser Operation
setLessThan = operation "slt" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t d 0 42

setLessThanUnsigned :: Parser Operation
setLessThanUnsigned = operation "sltu" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ registerEncoding 0 s t d 0 43

branchOnEqual :: Parser Operation
branchOnEqual = operation "beq" $ do
    s <- register
    argumentSeperator
    t <- register
    argumentSeperator
    i <- immediateNumber
    return $ immediateEncoding 4 s t i

branchOnNotEqual :: Parser Operation
branchOnNotEqual = operation "bne" $ do
    s <- register
    argumentSeperator
    t <- register
    argumentSeperator
    i <- immediateNumber
    return $ immediateEncoding 5 s t i

-- Parses the 'jr' command
jumpRegister :: Parser Operation
jumpRegister = operation "jr" $ do
    s <- register
    return $ jumpEncoding s 8

jumpAndLinkRegister :: Parser Operation
jumpAndLinkRegister = operation "jalr" $ do
    s <- register
    return $ jumpEncoding s 9
