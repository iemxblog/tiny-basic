module Main where

import Data.Text
import Data.Attoparsec.Text
import AST
import Parser

main :: IO ()
main = do
    s <- getContents
    case parseOnly program (pack s) of
        Left err -> putStrLn err
        Right v -> print v
