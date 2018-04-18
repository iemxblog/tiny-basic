module Main where

import Data.Text hiding (unlines, map, concatMap)
import System.Environment
import Data.Attoparsec.Text
import Control.Monad
import Parser
import AST
import Compiler
import CodeGenerator

usage :: IO ()
usage = do
    pn <- getProgName
    putStrLn $ "Usage : " ++ pn ++ " SOURCE_FILE.bas OUTPUT_FILE.s"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [sourceFileName, outputFileName] -> do
            source <- readFile sourceFileName
            case parseOnly program (pack source) of
                Left err -> putStrLn err
                Right p -> do
                    print p
                    putStrLn "****"
                    putStrLn $ getCompiledCode $ compile p
        _ -> usage
