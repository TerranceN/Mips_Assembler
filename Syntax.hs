module Syntax where

import Control.Monad.Reader
import Data.Functor.Identity
import Data.Word
import Data.Bits

data Metadata = Metadata { labels :: [(Label, Int)] }
data LineData = LineData { lineNum :: Int }

type ProgramCompiler a = Reader Metadata a
type LineCompiler a = ReaderT LineData (ReaderT Metadata Identity) a

type Register  = Int
type Label = String

data Location = LabelLocation Label
              | IntLocation Int

data Operation = Word Int
               | Add Register Register Register
               | Subtract Register Register Register
               | Multiply Register Register
               | MultiplyUnsigned Register Register
               | Divide Register Register
               | DivideUnsigned Register Register
               | MoveFromHigh Register
               | MoveFromLow Register
               | LoadImmediateAndSkip Register
               | LoadWord Register Register Int
               | StoreWord Register Register Int
               | SetLessThan Register Register Register
               | SetLessThanUnsigned Register Register Register
               | BranchOnEqual Register Register Location
               | BranchOnNotEqual Register Register Location
               | JumpRegister Register
               | JumpAndLinkRegister Register

type Line = ([Label], Operation)
type Program = [Line]

compileProgram :: Program -> [Word8]
compileProgram program = runReader (compile program 0) (generateMetadata program)
    where
        compile [] _ = return []
        compile ((_, lineOp):lines) n = do
            lineWords <- runReaderT (compileOperation lineOp) (LineData { lineNum = n })
            restWords <- compile lines (n + 1)
            return $ lineWords ++ restWords

generateMetadata :: Program -> Metadata
generateMetadata program = Metadata { labels = findLabels program }

findLabels :: Program -> [(Label, Int)]
findLabels program = findLabelsOnLine program 0
  where
    findLabelsOnLine ((lbls, _):lines) n =
        (map (\x -> (x, n)) lbls) ++ (findLabelsOnLine lines (n + 1))

-- Uses the register command encoding to generate an operation
registerEncoding :: Int -> Int -> Int -> Int -> Int -> Int -> [Word8]
registerEncoding op s t dest shift func =
    [ fromIntegral ((op `shiftL` 2) + (s `shiftR` 3))
    , fromIntegral ((s `shiftL` 5) + t)
    , fromIntegral ((dest `shiftL` 3) + (shift `shiftR` 2))
    , fromIntegral ((shift `shiftL` 6) + func)
    ]

-- Uses the immediate encoding to generate an operation
immediateEncoding :: Int -> Int -> Int -> Int -> [Word8]
immediateEncoding op s t i =
    [ fromIntegral ((op `shiftL` 2) + (s `shiftR` 3))
    , fromIntegral ((s `shiftL` 5) + t)
    , iHigh
    , iLow
    ]
  where
    iParts = octetsFrom16 (fromIntegral i)
    iHigh = iParts !! 0
    iLow = iParts !! 1

-- Uses the register command encoding to generate an operation
jumpEncoding :: Int -> Int -> [Word8]
jumpEncoding s op =
    [ fromIntegral (s `shiftR` 3)
    , fromIntegral (s `shiftL` 5)
    , 0
    , fromIntegral op
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
octetsFrom16 :: Word16 -> [Word8]
octetsFrom16 w =
    [ fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

errorFind :: Eq a => a -> [(a, b)] -> (a, b)
errorFind a [] = error $ "Cannot find label: "
errorFind a (p@(x, y):xs) =
    if (x == a)
        then p
        else errorFind a xs

compileOperation :: Operation -> LineCompiler [Word8]
compileOperation (Word i) = return $ octets $ fromIntegral i
compileOperation (Add d s t) = return $ registerEncoding 0 s t d 0 32
compileOperation (Subtract d s t) = return $ registerEncoding 0 s t d 0 34
compileOperation (Multiply s t) = return $ registerEncoding 0 s t 0 0 24
compileOperation (MultiplyUnsigned s t) = return $ registerEncoding 0 s t 0 0 25
compileOperation (Divide s t) = return $ registerEncoding 0 s t 0 0 26
compileOperation (DivideUnsigned s t) = return $ registerEncoding 0 s t 0 0 27
compileOperation (MoveFromHigh d) = return $ registerEncoding 0 0 0 d 0 16
compileOperation (MoveFromLow d) = return $ registerEncoding 0 0 0 d 0 18
compileOperation (LoadImmediateAndSkip d) = return $ registerEncoding 0 0 0 d 0 20
compileOperation (LoadWord s t i) = return $ immediateEncoding 35 s t i
compileOperation (StoreWord s t i) = return $ immediateEncoding 43 s t i
compileOperation (SetLessThan d s t) = return $ registerEncoding 0 s t d 0 42
compileOperation (SetLessThanUnsigned d s t) = return $ registerEncoding 0 s t d 0 43
compileOperation (BranchOnEqual s t (LabelLocation l)) = do
    lineNumber <- fmap lineNum $ ask
    (_, labelNum) <- fmap ((errorFind l) . (labels)) (lift ask)
    compileOperation (BranchOnEqual s t (IntLocation (labelNum - lineNumber)))
compileOperation (BranchOnEqual s t (IntLocation i)) = return $ immediateEncoding 4 s t i
compileOperation (BranchOnNotEqual s t (LabelLocation l)) = do
    lineNumber <- fmap lineNum $ ask
    (_, labelNum) <- fmap ((errorFind l) . (labels)) (lift ask)
    compileOperation (BranchOnNotEqual s t (IntLocation (labelNum - lineNumber)))
compileOperation (BranchOnNotEqual s t (IntLocation i)) = return $ immediateEncoding 5 s t i
compileOperation (JumpRegister s) = return $ jumpEncoding s 8
compileOperation (JumpAndLinkRegister s) = return $ jumpEncoding s 9
