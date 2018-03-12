module Variables (
    variables,
    checkVariables
) where

import qualified Data.Set as Set
import Data.List
import Control.Monad
import AST

class HasVars a where
    initializedVariables :: a -> Set.Set Var
    usedVariables :: a -> Set.Set Var

instance HasVars Program where
    initializedVariables (Program xs) = Set.unions (map initializedVariables xs)
    usedVariables (Program xs) = Set.unions (map usedVariables xs)

instance HasVars Line where
    initializedVariables (Line _ s) = initializedVariables s
    initializedVariables EmptyLine = Set.empty
    usedVariables (Line _ s) = usedVariables s
    usedVariables EmptyLine = Set.empty

instance HasVars Statement where
    initializedVariables (Print _) = Set.empty
    initializedVariables (If _ _ _ s) = initializedVariables s
    initializedVariables (Goto _) = Set.empty
    initializedVariables (Input xs) = Set.fromList xs
    initializedVariables (Let v _) = Set.singleton v
    initializedVariables (GoSub _) = Set.empty
    initializedVariables Return = Set.empty
    initializedVariables Clear = Set.empty
    initializedVariables List = Set.empty
    initializedVariables Run = Set.empty
    initializedVariables End = Set.empty

    usedVariables (Print xs) = Set.unions (map usedVariables xs)
    usedVariables (If e1 _ e2 s) = usedVariables e1 `Set.union` usedVariables e2 `Set.union` usedVariables s
    usedVariables (Goto e) = usedVariables e
    usedVariables (Input _) = Set.empty
    usedVariables (Let _ e) = usedVariables e
    usedVariables (GoSub e) = usedVariables e
    usedVariables Return = Set.empty
    usedVariables Clear = Set.empty
    usedVariables List = Set.empty
    usedVariables Run = Set.empty
    usedVariables End = Set.empty

instance HasVars Expr where
    initializedVariables (ExprString _) = Set.empty
    initializedVariables (ExprExpr _) = Set.empty
    
    usedVariables (ExprString _) = Set.empty
    usedVariables (ExprExpr e) = usedVariables e

instance HasVars Expression where
    initializedVariables (Expression _ _ _) = Set.empty
    
    usedVariables (Expression _ t1 xs) = usedVariables t1 `Set.union` (Set.unions $ map (\(_, t) -> usedVariables t) xs)

instance HasVars Term where
    initializedVariables (Term _ _) = Set.empty
    
    usedVariables (Term f1 xs) = usedVariables f1 `Set.union` (Set.unions $ map (\(_, f) -> usedVariables f) xs)

instance HasVars Factor where
    initializedVariables (VarFactor _) = Set.empty
    initializedVariables (NumberFactor _) = Set.empty
    initializedVariables (ExpressionFactor _) = Set.empty

    usedVariables (VarFactor v) = Set.singleton v
    usedVariables (NumberFactor _) = Set.empty
    usedVariables (ExpressionFactor e) = usedVariables e

variables :: Program -> [Var]
variables = Set.toAscList . initializedVariables 

uninitializedVariables :: Program -> Set.Set Var
uninitializedVariables p = usedVariables p `Set.difference` initializedVariables p

unusedVariables :: Program -> Set.Set Var
unusedVariables p = initializedVariables p `Set.difference` usedVariables p

checkVariables :: Program -> IO Bool
checkVariables p = do
    let uiv = uninitializedVariables p
    if uiv /= Set.empty then do
        putStrLn $ "Error : Variable(s) " ++ buildString uiv ++ " are never initialized before use"
        return False
    else do
        let uuv = unusedVariables p
        when (uuv /= Set.empty) $
            putStrLn $ "Warning : Variable(s) " ++ buildString uuv ++ " are initialized but never used" 
        return True
    where buildString = concat . intersperse ", " . map (\v -> [v]) . Set.toAscList
