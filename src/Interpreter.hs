module Main where

import Data.Text hiding (length)
import Data.Attoparsec.Text
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Data.Maybe
import System.Environment
import System.IO
import AST
import Parser

type LineNumber = Int
type Variables = Map.Map Char Int
type Labels = Map.Map Label LineNumber
type CallStack = [LineNumber]
type RunState = Bool
type Environment = (Program, Labels)
type ProgramState = (LineNumber, Variables, CallStack, RunState)

type Interpreter a = ErrorT String (ReaderT Environment (StateT ProgramState IO)) a

interpreterError :: String -> Interpreter a
interpreterError msg = getCurrentLine >>= \l -> throwError $ "Line " ++ show (l+1) ++ ": " ++ msg

getProgram :: Interpreter Program
getProgram = liftM fst (lift ask)

getLabels :: Interpreter Labels
getLabels = liftM snd (lift ask)

setCurrentLine :: Int -> Interpreter ()
setCurrentLine l = do
    (_, vs, cs, rn) <- lift $ lift get
    lift $ lift $ put (l, vs, cs, rn)
    
incLine :: Interpreter ()
incLine = do
    l <- getCurrentLine
    setCurrentLine (l+1)

getCurrentLine :: Interpreter Int
getCurrentLine = liftM (\(l, _, _, _) -> l) $ lift $ lift get

getVariables :: Interpreter Variables
getVariables = liftM (\(_, vs, _, _) -> vs) $ lift $ lift get

setVariables :: Variables -> Interpreter ()
setVariables vs = do
    (l, _, cs, rn) <- lift $ lift get
    lift $ lift $ put (l, vs, cs, rn)

getVariable :: Var -> Interpreter Int
getVariable vn = do
    vs <- getVariables
    case Map.lookup vn vs of
        Nothing -> interpreterError $ "Unknown variable " ++ [vn]
        Just vv -> return vv


setVariable :: Var -> Int -> Interpreter ()
setVariable var value = do
    vs <- getVariables
    let vs2 = Map.insert var value vs
    setVariables vs2 

getCallStack :: Interpreter CallStack
getCallStack = liftM (\(_, _, cs, _) -> cs) $ lift $ lift get

setCallStack :: CallStack -> Interpreter ()
setCallStack cs = do
    (l, vs, _, rn) <- lift $ lift $ get
    lift $ lift $ put (l, vs, cs, rn)

push :: Interpreter ()
push = do
    l <- getCurrentLine
    cs <- getCallStack
    setCallStack (l:cs)

pop :: Interpreter ()
pop = do
    cs <- getCallStack
    case cs of
        [] -> interpreterError "Popped empty stack"
        (l:ncs) -> setCurrentLine l >> setCallStack ncs

isRunning :: Interpreter RunState
isRunning = liftM (\(_, _, _, rn) -> rn) $ lift $ lift get

stop :: Interpreter ()
stop = do
    (l, vs, cs, _) <- lift $ lift $ get 
    lift $ lift $ put (l, vs, cs, False)

runProgram :: Interpreter ()
runProgram = do
    l <- getCurrentLine
    pls <- liftM getProgramLines getProgram
    r <- isRunning
    if l < length pls && r then do
        interpretLine (pls !! l)
        runProgram
    else
        return ()

interpretLine :: Line -> Interpreter ()
interpretLine (Line _ s) = interpretStatement s >> incLine
interpretLine EmptyLine = incLine

interpretStatement :: Statement -> Interpreter ()
interpretStatement (Print xs) = do
    mapM_ interpretExpr xs
    lift $ lift $ lift $ putStr "\n"
interpretStatement (If e1 ro e2 s) = do
    e1r <- evalExpression e1
    e2r <- evalExpression e2
    case ro of
        LessThan -> if e1r < e2r then interpretStatement s else return ()
        Different -> if e1r /= e2r then interpretStatement s else return ()
        LessThanOrEqual -> if e1r <= e2r then interpretStatement s else return ()
        GreaterThan -> if e1r > e2r then interpretStatement s else return ()
        GreaterThanOrEqual -> if e1r >= e2r then interpretStatement s else return ()
        Equal -> if e1r == e2r then interpretStatement s else return ()
interpretStatement (Goto e) = goto e 
interpretStatement (Input vs) = mapM_ inputVariable vs
interpretStatement (Let vn e) = evalExpression e >>= setVariable vn
interpretStatement (GoSub e) = push >> goto e
interpretStatement Return = pop
interpretStatement Clear = setVariables Map.empty
interpretStatement End = stop
   
goto :: Expression -> Interpreter ()
goto e = do
    er <- evalExpression e
    ls <- getLabels
    case Map.lookup er ls of
        Just l -> setCurrentLine l >> runProgram
        Nothing -> interpreterError $ "No such label : " ++ show er

inputVariable :: Var -> Interpreter ()
inputVariable v = do
    lift $ lift $ lift $ putStr (v:" = ")
    lift $ lift $ lift $ hFlush stdout
    line <- lift $ lift $ lift getLine
    case parseOnly decimal (pack line) of
        Left err -> interpreterError "Integer expected"
        Right value -> setVariable v value

interpretExpr :: Expr -> Interpreter ()
interpretExpr (ExprString xs) = lift $ lift $ lift $  putStr xs
interpretExpr (ExprExpr e) = do
    er <- evalExpression e
    lift $ lift $ lift $ putStr $ show er

evalExpression :: Expression -> Interpreter Int
evalExpression (Expression s t xs) = do
    t1 <- liftM (evalSign s 0) (evalTerm t)
    foldM evalExpressionPart t1 xs    

evalExpressionPart :: Int -> (Sign, Term) -> Interpreter Int
evalExpressionPart tv1 (s, t2) = do
    tv2 <- evalTerm t2
    return $ evalSign s tv1 tv2

evalSign :: Sign -> Int -> Int -> Int
evalSign Plus = (+)
evalSign Minus = (-)

evalTerm :: Term -> Interpreter Int
evalTerm (Term fa xs) = do
    far <- evalFactor fa
    foldM evalTermPart far xs

evalTermPart :: Int -> (MultSymbol, Factor) -> Interpreter Int
evalTermPart fa1 (ms, fa2) = do
    far2 <- evalFactor fa2
    return $ evalMultSymbol ms fa1 far2

evalMultSymbol :: MultSymbol -> Int -> Int -> Int 
evalMultSymbol Mult = (*)
evalMultSymbol Div = div

evalFactor :: Factor -> Interpreter Int
evalFactor (VarFactor v) = getVariable v
evalFactor (NumberFactor n) = return n
evalFactor (ExpressionFactor e) = evalExpression e


calculateLabels' :: LineNumber -> Program -> Labels
calculateLabels' _ (Program []) = Map.empty
calculateLabels' ln (Program ((Line (Just la) _):xs)) = case la `Map.member` remaining of
        False -> Map.insert la ln remaining
        True -> error $ "Duplicate label " ++ show la ++ "on lines " ++ show ln ++ " and " ++ show (fromJust (Map.lookup la remaining))
    where remaining = calculateLabels' (ln+1) (Program xs)
calculateLabels' ln (Program ((Line Nothing _):xs)) = calculateLabels' (ln+1) (Program xs)
calculateLabels' ln (Program (EmptyLine:xs)) = calculateLabels' (ln+1) (Program xs)

calculateLabels :: Program -> Map.Map Label LineNumber
calculateLabels = calculateLabels' 0


usage :: IO ()
usage = do
    pn <- getProgName
    putStrLn $ "Usage : " ++ pn ++ " SOURCE_FILE.bas"

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [sourceFileName] -> do 
            source <- readFile sourceFileName
            case parseOnly program (pack source) of
                Left err -> putStrLn err
                Right v -> do
                    print v 
                    putStrLn "Running program..."
                    r <- evalStateT (runReaderT (runErrorT runProgram) (v, calculateLabels v)) (0, Map.empty, [], True)
                    case r of
                        Left err -> putStrLn $ "Program failed with error : " ++ err
                        Right () -> putStrLn "Program terminated without error"
        _ -> usage
