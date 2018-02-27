{-# LANGUAGE OverloadedStrings #-}
module Parser (
    program
) where

import Data.Attoparsec.Text
import Data.Text
import Control.Applicative
import AST


token :: Parser a -> Parser a
token p = do
    skipSpace
    r <- p
    skipSpace
    return r

symbol :: Text -> Parser Text
symbol = token . string

program :: Parser Program
program = many' line

line :: Parser Line
line = try labeledLine <|> nonLabeledLine

labeledLine :: Parser Line
labeledLine = do
    n <- decimal
    s <- statement
    char '\n'
    return $ Line (Just n) s

nonLabeledLine :: Parser Line
nonLabeledLine = do
    s <- statement 
    char '\n'
    return $ Line Nothing s


statement :: Parser Statement
statement = 
    try printStatement
    <|> try ifStatement
    <|> try gotoStatement
    <|> try inputStatement
    <|> try letStatement
    <|> try gosubStatement
    <|> try returnStatement
    <|> try clearStatement
    <|> try listStatement
    <|> try runStatement
    <|> endStatement

printStatement :: Parser Statement
printStatement = do
    string "PRINT "
    es <- exprList
    return $ Print es

ifStatement :: Parser Statement
ifStatement = do
    string "IF "
    e1 <- expression
    string " "
    r <- relop
    string " "
    e2 <- expression
    string " THEN "
    s <- statement
    return $ If e1 r e2 s

gotoStatement :: Parser Statement
gotoStatement = do
    string "GOTO "
    e <- expression
    return $ Goto e

inputStatement :: Parser Statement
inputStatement = do
    string "INPUT "
    vs <- varList
    return $ Input vs

letStatement :: Parser Statement
letStatement = do
    string "LET "
    v <- var
    string " = "
    e <- expression
    return $ Let v e

gosubStatement :: Parser Statement
gosubStatement = do
    string "GOSUB "
    e <- expression
    return $ GoSub e

returnStatement :: Parser Statement
returnStatement = string "RETURN" >> return Return

clearStatement :: Parser Statement
clearStatement = string "CLEAR" >> return Clear

listStatement :: Parser Statement
listStatement = string "LIST" >> return List

runStatement :: Parser Statement
runStatement = string "RUN" >> return Run

endStatement :: Parser Statement
endStatement = string "END" >> return End

exprList :: Parser [Expr]
exprList = many1 expr

expr :: Parser Expr
expr =
    try (bstring >>= \s -> return $ ExprString s) 
    <|> (expression >>= \e -> return $ ExprExpr e)

varList :: Parser [Var]
varList = var `sepBy` (char ',')

expression :: Parser Expression
expression = do
    ms <- try sign <|> return Plus
    t <- term
    ps <- many' expressionPart
    return $ Expression ms t ps

expressionPart :: Parser (Sign, Term)
expressionPart = do
    s <- sign
    t <- term
    return (s, t)

term :: Parser Term
term = do
    f <- factor
    ps <- many' termPart
    return $ Term f ps

termPart :: Parser (MultSymbol, Factor)
termPart = do
    s <- multSymbol
    f <- factor
    return (s, f)

factor :: Parser Factor
factor = 
    try varFactor
    <|> try numberFactor
    <|> expressionFactor

varFactor :: Parser Factor
varFactor = do
    v <- var
    return $ VarFactor v

numberFactor :: Parser Factor
numberFactor = do
    i <- decimal
    return $ NumberFactor i

expressionFactor :: Parser Factor
expressionFactor = do
    char '('
    e <- expression
    char ')'
    return $ ExpressionFactor e

sign :: Parser Sign
sign = try (char '+' >> return Plus) <|> (char '-' >> return Minus)

multSymbol :: Parser MultSymbol
multSymbol = try (char '*' >> return Mult) <|> (char '/' >> return Div)

var :: Parser Var
var = do
    c <- satisfy (inClass "A-Z")
    return $ c

relop :: Parser Relop
relop = 
    try (string "<>" >> return Different)
    <|> try (string "<=" >> return LessThanOrEqual)
    <|> try (char '<' >> return LessThan)
    <|> try (string ">=" >> return GreaterThanOrEqual)
    <|> try (char '>' >> return GreaterThan)
    <|> (char '=' >> return Equal)

bstring :: Parser String
bstring = do
    char '"'
    s <- many' (digit <|> letter)
    char '"'
    return $ s
