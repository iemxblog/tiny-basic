module Main where

import Data.Text
import System.Environment
import Data.Attoparsec.Text
import Parser
import AST
import Variables


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
                Right v -> do
                    print v
                    putStrLn "****"
                    putStrLn "initialized variables : "
                    print $ initializedVariables v
                    putStrLn "used variables : "
                    print $ usedVariables v
        _ -> usage
