{-# LANGUAGE OverloadedStrings #-}
module Parser (
    program
) where

import Data.Attoparsec.Text
import Data.Text
import Control.Applicative
import Control.Monad
import AST


token :: Parser a -> Parser a
token p = do
    many (char ' ')
    r <- p
    many (char ' ')
    return r

symbol :: Text -> Parser Text
symbol = token . string

program :: Parser Program
program = liftM Program $ many' line

line :: Parser Line
line = try labeledLine <|> try nonLabeledLine <|> emptyLine

labeledLine :: Parser Line
labeledLine = do
    n <- token decimal
    s <- statement
    symbol "\n"
    return $ Line (Just n) s

nonLabeledLine :: Parser Line
nonLabeledLine = do
    s <- statement 
    symbol "\n"
    return $ Line Nothing s

emptyLine :: Parser Line
emptyLine = symbol "\n" >> return EmptyLine

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
    symbol "PRINT"
    es <- exprList
    return $ Print es

ifStatement :: Parser Statement
ifStatement = do
    symbol "IF"
    e1 <- expression
    r <- relop
    e2 <- expression
    symbol "THEN"
    s <- statement
    return $ If e1 r e2 s

gotoStatement :: Parser Statement
gotoStatement = do
    symbol "GOTO"
    e <- expression
    return $ Goto e

inputStatement :: Parser Statement
inputStatement = do
    symbol "INPUT"
    vs <- varList
    return $ Input vs

letStatement :: Parser Statement
letStatement = do
    symbol "LET"
    v <- var
    symbol "="
    e <- expression
    return $ Let v e

gosubStatement :: Parser Statement
gosubStatement = do
    symbol "GOSUB"
    e <- expression
    return $ GoSub e

returnStatement :: Parser Statement
returnStatement = symbol "RETURN" >> return Return

clearStatement :: Parser Statement
clearStatement = symbol "CLEAR" >> return Clear

listStatement :: Parser Statement
listStatement = symbol "LIST" >> return List

runStatement :: Parser Statement
runStatement = symbol "RUN" >> return Run

endStatement :: Parser Statement
endStatement = symbol "END" >> return End

exprList :: Parser [Expr]
exprList = many1 expr

expr :: Parser Expr
expr =
    try (bstring >>= \s -> return $ ExprString s) 
    <|> (expression >>= \e -> return $ ExprExpr e)

varList :: Parser [Var]
varList = var `sepBy` (symbol ",")

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
    i <- token decimal
    return $ NumberFactor i

expressionFactor :: Parser Factor
expressionFactor = do
    symbol "("
    e <- expression
    symbol ")"
    return $ ExpressionFactor e

sign :: Parser Sign
sign = try (symbol "+" >> return Plus) <|> (symbol "-" >> return Minus)

multSymbol :: Parser MultSymbol
multSymbol = try (symbol "*" >> return Mult) <|> (symbol "/" >> return Div)

var :: Parser Var
var = token $ do
    c <- satisfy (inClass "A-Z")
    return $ c

relop :: Parser Relop
relop = 
    try (symbol "<>" >> return Different)
    <|> try (symbol "<=" >> return LessThanOrEqual)
    <|> try (symbol "<" >> return LessThan)
    <|> try (symbol ">=" >> return GreaterThanOrEqual)
    <|> try (symbol ">" >> return GreaterThan)
    <|> (symbol "=" >> return Equal)

bstring :: Parser String
bstring = token $ do
    char '"'
    s <- many' (digit <|> letter)
    char '"'
    return $ s
