module Parser
( parseProgram
) where

import Control.Monad
import Data.Word
import Data.Bits
import Data.Char
import Text.ParserCombinators.Parsec hiding (Line)

import Syntax

-- parses a single decimal digit
numbers :: Parser Char
numbers = oneOf ['0'..'9']

-- parses a single hex digit
hexNumbers :: Parser Char
hexNumbers = (try numbers) <|> oneOf (['A'..'F'] ++ ['a'..'f'])

parseProgram :: Parser Program
parseProgram = (newline >> parseProgram) <|> ((eof >> return []) <|> parseLines)

maybeParse :: Parser a -> a -> Parser a
maybeParse p a = ((try p) <|> return a)

eol :: Parser ()
eol = (newline <|> (eof >> return '\0')) >> return ()

parseLines :: Parser Program
parseLines = do
    maybeLine <- parseLine
    case maybeLine of
        Nothing -> parseProgram
        Just syntaxLine -> do
            rest <- parseProgram
            return $ syntaxLine:rest

parseLine :: Parser (Maybe Line)
parseLine =
    operationLine <|> commentLine
  where
    commentLine = do
        (try (comment >> newline)) <|> newline
        return Nothing
    operationLine = do
        labels <- (try parseLabels) <|> return []
        op <- parseOperation
        (try (comment >> eol)) <|> eol
        return $ Just (labels, op)

parseLabels :: Parser [String]
parseLabels = do
    labels <- many1 parseLabel
    char ':'
    return labels

parseLabel :: Parser String
parseLabel = do
    skipMany space
    first <- firstChar
    rest <- restChars
    return (first:rest)
  where
    firstChars = ['a'..'z'] ++ ['A'..'Z']
    firstChar = oneOf firstChars
    restChars = many (oneOf (firstChars ++ ['0'..'9']))

parseOperation :: Parser Operation
parseOperation = do
    foldr1 (\a b -> (try a) <|> b) operations

comment :: Parser ()
comment = do
    skipMany space
    char ';'
    skipMany (noneOf "\n")

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

parsePositiveNumber :: Parser Int
parsePositiveNumber = do
    numString <- many1 (oneOf ['0'..'9'])
    return $ read numString

parseInteger :: Parser Int
parseInteger = (try (char '-' >> fmap (* (-1)) parsePositiveNumber))
           <|> parsePositiveNumber

decimalNumber :: Parser Word32
decimalNumber = do
    number <- parseInteger
    if number > 0xEFFFFFFF || number < (-0x80000000)
        then error "Invalid number"
        else return (fromIntegral number)

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
          <|> (try decimalNumber)
    return (Word (fromIntegral number))

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
    number <- parseInteger
    if number > 0x7FFF || number < (-0x8000)
        then error "Not a valid 16-bit number"
        else return number

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
    return $ Add d s t

-- Parses the 'subtract command'
subtract :: Parser Operation
subtract = operation "sub" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ Subtract d s t

multiply :: Parser Operation
multiply = operation "mult" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ Multiply s t

multiplyUnsigned :: Parser Operation
multiplyUnsigned = operation "multu" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ MultiplyUnsigned s t

divide :: Parser Operation
divide = operation "div" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ Divide s t

divideUnsigned :: Parser Operation
divideUnsigned = operation "divu" $ do
    s <- register
    argumentSeperator
    t <- register
    return $ DivideUnsigned s t

moveFromHigh :: Parser Operation
moveFromHigh = operation "mfhi" $ do
    d <- register
    return $ MoveFromHigh d

moveFromLow :: Parser Operation
moveFromLow = operation "mflo" $ do
    d <- register
    return $ MoveFromLow d

loadImmediateAndSkip :: Parser Operation
loadImmediateAndSkip = operation "lis" $ do
    d <- register
    return $ LoadImmediateAndSkip d

loadWord :: Parser Operation
loadWord = operation "lw" $ do
    t <- register
    argumentSeperator
    (i, s) <- bracketRegister
    return $ LoadWord s t i

storeWord :: Parser Operation
storeWord = operation "sw" $ do
    t <- register
    argumentSeperator
    (i, s) <- bracketRegister
    return $ StoreWord s t i

setLessThan :: Parser Operation
setLessThan = operation "slt" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ SetLessThan d s t

setLessThanUnsigned :: Parser Operation
setLessThanUnsigned = operation "sltu" $ do
    d <- register
    argumentSeperator
    s <- register
    argumentSeperator
    t <- register
    return $ SetLessThanUnsigned d s t

branchOnEqual :: Parser Operation
branchOnEqual = operation "beq" $ do
    s <- register
    argumentSeperator
    t <- register
    argumentSeperator
    i <- immediateNumber
    return $ BranchOnEqual s t (IntLocation i)
    
branchOnNotEqual :: Parser Operation
branchOnNotEqual = operation "bne" $ do
    s <- register
    argumentSeperator
    t <- register
    argumentSeperator
    i <- immediateNumber
    return $ BranchOnNotEqual s t (IntLocation i)

-- Parses the 'jr' command
jumpRegister :: Parser Operation
jumpRegister = operation "jr" $ do
    s <- register
    return $ JumpRegister s

jumpAndLinkRegister :: Parser Operation
jumpAndLinkRegister = operation "jalr" $ do
    s <- register
    return $ JumpAndLinkRegister s
