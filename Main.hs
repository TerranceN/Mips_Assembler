module Main where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Text.ParserCombinators.Parsec

import Parser

main = do
    input <- hGetLine stdin
    case parse parseOp "parseOp" input of
        Left error -> putStrLn ("Error: " ++ (show error))
        Right result -> do
            BL.putStr $ BL.pack result
