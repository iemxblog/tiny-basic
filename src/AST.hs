module AST (
    Program(..),
    Label,
    Line(..),
    Statement(..),
    Expr(..),
    Expression(..),
    Term(..),
    Factor(..),
    Sign(..),
    MultSymbol(..),
    Var(..),
    Relop(..)            
) where

import Data.List

newtype Program = Program {getProgramLines :: [Line] } deriving Eq

type Label = Int

data Line = Line (Maybe Label) Statement deriving Eq

data Statement =
    Print [Expr] 
    | If Expression Relop Expression Statement
    | Goto Expression
    | Input [Var]
    | Let Var Expression
    | GoSub Expression
    | Return
    | Clear
    | List
    | Run
    | End
    deriving Eq

data Expr = ExprString String | ExprExpr Expression deriving Eq

data Expression = Expression Sign Term [(Sign, Term)] deriving Eq

data Term = Term Factor [(MultSymbol, Factor)] deriving Eq

data Factor = VarFactor Var | NumberFactor Int | ExpressionFactor Expression deriving Eq

data Sign = Plus | Minus deriving Eq
data MultSymbol = Mult | Div deriving Eq

type Var = Char

data Relop =
    LessThan
    | Different
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    deriving Eq

instance Show Program where
    show (Program xs) = unlines . map show $ xs

instance Show Line where
    show (Line ml s) = maybe "" ((++ " ") . show) ml ++ show s

instance Show Statement where
    show (Print xs) = "PRINT " ++ (concat . intersperse ", " . map show) xs
    show (If e1 ro e2 s) = "IF " ++ show e1 ++ " " ++ show ro ++ " " ++ show e2 ++ " THEN " ++ show s
    show (Goto e) = "GOTO " ++ show e
    show (Input xs) = "INPUT " ++ (concat . intersperse ", " . map (\x -> [x])) xs
    show (Let v e) = "LET " ++ [v] ++ " = " ++ show e
    show (GoSub e) = "GOSUB " ++ show e
    show Return = "RETURN"
    show Clear = "CLEAR"
    show List = "LIST"
    show Run = "RUN"
    show End = "END"
    

instance Show Expr where
    show (ExprString s) = show s
    show (ExprExpr e) = show e

instance Show Expression where
    show (Expression s t xs) = showSign s ++ show t ++ concatMap f xs
        where
            showSign Plus = ""
            showSign Minus = "-"
            f (s, t) = show s ++ show t

instance Show Term where
    show (Term fa xs) = show fa ++ (unwords . map f) xs
        where f (s, t) = show s ++ show t


instance Show Factor where
    show (VarFactor v) = [v]
    show (NumberFactor i) = show i
    show (ExpressionFactor e) = "(" ++ show e ++ ")"

instance Show Sign where
    show Plus = "+"
    show Minus = "-"

instance Show MultSymbol where
    show Mult = "*"
    show Div = "/"

instance Show Relop where
    show LessThan = "<"
    show Different = "<>"
    show LessThanOrEqual = "<="
    show GreaterThan = ">"
    show GreaterThanOrEqual = ">="
    show Equal = "="
