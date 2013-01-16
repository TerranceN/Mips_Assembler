module Main where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Text.ParserCombinators.Parsec

import Parser
import Syntax

main = do
    input <- hGetContents stdin
    case parse parseProgram "parseOp" input of
        Left error -> putStrLn ("Error: " ++ (show error))
        Right result -> do
            BL.putStr $ BL.pack $ compileProgram result
