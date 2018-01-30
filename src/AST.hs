module AST (
    Program,
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

type Program = [Line]

data Line = NumberedLine Int Statement | Line Statement deriving (Eq, Show)
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
    deriving (Eq, Show)

data Expr = ExprString String | ExprExpr Expression deriving (Eq, Show)

data Expression = Expression Sign Term [(Sign, Term)] deriving (Eq, Show)

data Term = Term Factor [(MultSymbol, Factor)] deriving (Eq, Show)

data Factor = VarFactor Var | NumberFactor Int | ExpressionFactor Expression deriving (Eq, Show)

data Sign = Plus | Minus deriving (Eq, Show)
data MultSymbol = Mult | Div deriving (Eq, Show)

data Var = Var Char deriving (Eq, Show)

data Relop =
    LessThan
    | Different
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | Equal
    deriving (Eq, Show)
