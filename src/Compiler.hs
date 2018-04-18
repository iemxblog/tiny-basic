module Compiler (
    getCompiledCode,
    Compilable(..)
) where

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.State
import CodeGenerator
import AST

type CompilerState = (Map.Map String Int, Int) -- List of numbered strings, and "if" counter

type Compiler a = State CompilerState a

getStringMap :: Compiler (Map.Map String Int)
getStringMap = liftM fst get

putStringMap :: Map.Map String Int -> Compiler ()
putStringMap sm = modify (\(_, ic) -> (sm, ic))
   
getIfCounter :: Compiler Int
getIfCounter = do
    c <- liftM snd get
    modify (\(sm, ic) -> (sm, ic+1))
    return c

getStringName :: String -> Compiler String
getStringName sv = do
    m <- getStringMap
    let nsi = Map.foldr max 0 m + 1   -- nsi : new string index
    case Map.lookup sv m of
        Just i -> return $ "string" ++ show i
        Nothing -> putStringMap (Map.insert sv nsi m) >> return ("string" ++ show nsi)
    

getCompiledCode :: Compiler ASMCode -> String
getCompiledCode ac = genCode $ evalState ac (Map.empty, 0)

class Compilable a where
    compile :: a -> Compiler ASMCode


instance Compilable Program where
    compile (Program ls) = liftM mconcat (mapM compile ls) >>= \cp -> return $ aMain <> cp <> endMain

instance Compilable Line where
    compile li@(Line (Just l) st) = do
        c <- compile st
        return $ lineComment (show li) <> gLabel l <> c
    compile li@(Line Nothing st) = compile st >>= \cst -> return $ lineComment (show li) <> cst
    compile EmptyLine = return mempty

instance Compilable Statement where
    compile (Print []) = return printNewLine
    compile (Print (ExprString sv : xs)) = do
        sn <- getStringName sv
        c <- compile (Print xs)
        return $ printString sn sv <> c
    compile (Print (ExprExpr e : xs)) = do
        ce <- compile e
        cs <- compile (Print xs)
        return $ ce <> printInt <> cs
    compile (If e1 ro e2 st) = do
        ce1 <- compile e1
        ce2 <- compile e2
        ic <- getIfCounter
        cst <- compile st
        let cro = case ro of
                    LessThan -> lt ic
                    Different -> different ic
                    LessThanOrEqual -> lte ic
                    GreaterThan -> gt ic
                    GreaterThanOrEqual -> gte ic
                    Equal -> eq ic
        return $ ce1 <> ce2 <> cro <> cst <> aElse ic
    compile (Goto e) = compile e >>= \ce -> return $ ce <> goto
    compile (Input []) = return mempty
    compile (Input (v:vs)) = do
        civs <- compile (Input vs)
        return $ input v <> civs
    compile (Let v e) = do
        ce <- compile e
        return $ ce <> store v
    compile (GoSub e) = compile e >>= \ce -> return $ ce <> gosub
    compile Return = return aReturn

instance Compilable Expression where
    compile (Expression s t xs) = do
        ct <- compile t                 -- ct : compiled term
        let cts = case s of             -- cts : signed compiled term
                    Plus -> cts
                    Minus -> cts <> neg
        foldM (\ce (s, t) -> compile t >>= \ct2 -> return (ce <> ct2 <> if s == Plus then add else sub)) ct xs

instance Compilable Term where
    compile (Term f xs) = do
        cf <- compile f
        foldM (\ct (s, f) -> compile f >>= \cf2 -> return (ct <> cf2 <> if s == Mult then mul else idiv)) cf xs

instance Compilable Factor where
    compile (VarFactor v) = return $ pushV v
    compile (NumberFactor i) = return $ pushI i
    compile (ExpressionFactor e) = compile e
