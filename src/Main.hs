module Main where

import Data.Text hiding (length)
import Data.Attoparsec.Text
import qualified Data.Map as Map
import Control.Monad.State
import AST
import Parser

type LineNumber = Int
type Variables = Map.Map Char Int
type Environment = (LineNumber, Variables)

incLine :: State Environment ()
incLine = do
    (l, e) <- get
    put (l+1, e)

runProgram :: Program -> State Environment (IO ())
runProgram p = do
    (l, _) <- get
    if l < length p then do
        a <- interpretLine $ p !! l
        as <- runProgram p 
        return (a >> as)
    else
        return (return ())

interpretLine :: Line -> State Environment (IO ())
interpretLine (Line _ s) = interpretStatement s

interpretStatement :: Statement -> State Environment (IO ())
interpretStatement (Print xs) = do
    a <- liftM sequence_ (mapM interpretExpr xs)
    incLine
    return a
interpretStatement (If e1 ro e2 s) = do
    e1r <- evalExpression e1
    e2r <- evalExpression e2
    case ro of
        LessThan -> if e1r < e2r then interpretStatement s else return (return ())
    

interpretExpr :: Expr -> State Environment (IO ())
interpretExpr (ExprString xs) = return $ putStr xs
interpretExpr (ExprExpr e) = liftM (putStr . show) $ evalExpression e

evalExpression :: Expression -> State Environment Int
evalExpression (Expression s t xs) = do
    t1 <- liftM (evalSign s 0) (evalTerm t)
    foldM evalExpressionPart t1 xs    

evalExpressionPart :: Int -> (Sign, Term) -> State Environment Int
evalExpressionPart tv1 (s, t2) = do
    tv2 <- evalTerm t2
    return $ evalSign s tv1 tv2

evalSign :: Sign -> Int -> Int -> Int
evalSign Plus = (+)
evalSign Minus = (-)

evalTerm :: Term -> State Environment Int
evalTerm (Term fa xs) = do
    far <- evalFactor fa
    foldM evalTermPart far xs

evalTermPart :: Int -> (MultSymbol, Factor) -> State Environment Int
evalTermPart fa1 (ms, fa2) = do
    far2 <- evalFactor fa2
    return $ evalMultSymbol ms fa1 far2

evalMultSymbol :: MultSymbol -> Int -> Int -> Int 
evalMultSymbol Mult = (*)
evalMultSymbol Div = div

evalFactor :: Factor -> State Environment Int
evalFactor (VarFactor v) = do
    (_, vs) <- get
    case Map.lookup v vs of
        Just i -> return i
        Nothing -> error $ "Unknown variable " ++ [v]
evalFactor (NumberFactor n) = return n
evalFactor (ExpressionFactor e) = evalExpression e

main :: IO ()
main = do
    s <- getContents
    case parseOnly program (pack s) of
        Left err -> putStrLn err
        Right v -> print v >> evalState (runProgram v) (0, Map.empty)
