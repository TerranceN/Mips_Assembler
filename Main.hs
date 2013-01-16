module Main where

import System.IO
import Data.List
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Text.ParserCombinators.Parsec

import Parser

main = do
    input <- hGetContents stdin
    let inputLines = lines input
    labels <- parseLabels inputLines 0
    operations <- parseOperations labels inputLines 0
    mapM_ (BL.putStr . BL.pack) operations
  where
    parseLabels :: [String] -> Int -> IO [Label]
    parseLabels [] _ = return []
    parseLabels (x:xs) n =
        case parse parseLbl "parselbl" x of
            Left parseError -> error ("Error: " ++ (show parseError))
            Right result -> do
                rst <- parseLabels xs (n + 1)
                if not $ null (filter (\(lbl, _) -> lbl `elem` result) rst)
                    then error "duplicate label declaration"
                    else return ()
                return $ (map (\x -> (x, n)) (nub result)) ++ rst
    parseOperations :: [Label] -> [String] -> Int -> IO [Operation]
    parseOperations _ [] n = return []
    parseOperations labels (x:xs) n = do
        ops <- parseLine labels x n
        if null ops
            then parseOperations labels xs n
            else do
                rest <- parseOperations labels xs (n + 1)
                return (ops:rest)
    parseLine :: [Label] -> String -> Int -> IO Operation
    parseLine labels line lineNum = 
        case parse (parseOp labels lineNum) "parseOp" line of
            Left parseError -> error ("Error: " ++ (show parseError))
            Right result -> return result
