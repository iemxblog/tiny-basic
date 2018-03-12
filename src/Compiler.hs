module Main where

import Data.Text hiding (unlines, map)
import System.Environment
import Data.Attoparsec.Text
import Control.Monad
import Parser
import AST
import Variables


assemblySource :: Program -> String
assemblySource p = dataSection p ++ textSection p

dataSection :: Program -> String
dataSection p = ".data\n.balign4\n" ++ (unlines . map (\v -> "var" ++ [v] ++ ":\n    .word 0") $ variables p)

textSection :: Program -> String
textSection p = ".text\n" ++ relocation p

relocation :: Program -> String
relocation = unlines . map (\v -> "addr_of_var" ++ [v] ++ ": .word var" ++ [v]) . variables

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
                    b <- checkVariables p
                    when b $ return ()
                    putStrLn $ assemblySource p
                    
        _ -> usage
