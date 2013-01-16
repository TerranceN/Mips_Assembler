module Main where

import System.IO
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Text.ParserCombinators.Parsec

import Parser

main = do
    input <- hGetContents stdin
    operations <- mapM parseLine (lines input)
    mapM_ (BL.putStr . BL.pack) operations
  where
    parseLine line = 
        case parse parseOp "parseOp" line of
            Left parseError -> error ("Error: " ++ (show parseError))
            Right result -> return result
