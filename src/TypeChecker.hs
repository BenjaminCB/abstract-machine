module TypeChecker where

import AST
import RecursionSchemes
import Auxiliary

import Data.Map qualified as M
import Control.Monad.State.Lazy

data Type = S_Type SType
          | E_Type EType
          deriving (Show, Eq)

data SType = Number Int
           | Variable String
           | Addition SType SType
           | Multiplication String SType
           | Maximum SType SType
           deriving (Show, Eq)

data EType = Cost SType
           | Function [String] String
           | Record [(String, EType)]
           deriving (Show, Eq)

type TypeEnv = M.Map String Type

instance LUB Type where
    (\/) = undefined

instance LUB (M.Map String Type) where
    (\/) = undefined

(<:) :: Ord k => M.Map k v -> (k, v) -> M.Map k v
m <: (k, v) = M.insert k v m

(<<:) :: Ord k => M.Map k v -> [(k, v)] -> M.Map k v
m <<: [] = m
m <<: (x:xs) = m <: x <<: xs

fresh :: String -> StateT Int (Either String) String
fresh str = do
    s <- get
    put (s + 1)
    return $ str ++ "_" ++ show s

envLookup :: String -> TypeEnv -> Either String Type
envLookup k env = case M.lookup k env of
    Just v -> Right v
    Nothing -> Left $ "envLookup: key not found: " ++ k

srcFileTypeCheck :: Term SrcFile -> TypeEnv -> Either String (SType, TypeEnv)
srcFileTypeCheck srcFile env = cata f srcFile where
    f (SrcFile is ss es) = undefined

importTypeCheck :: Import (TypeEnv -> Either String (SType, TypeEnv)) -> TypeEnv -> Either String (SType, TypeEnv)
importTypeCheck (ImportStar ident s) env = do
    (t, env') <-  s env
    exports <- envLookup "epsilon" env'
    return (Addition t (Number 3), env' <: (ident, exports) <: ("epsilon", E_Type $ Record []))
importTypeCheck (ImportList ids s) env = do
    (t, env') <- s env
    exports <- envLookup "epsilon" env'
    case exports of
        E_Type (Record fields) -> do
            -- TODO does not take into account if some id is not in exports
            let matches = foldr (\(k, v) acc -> if k `elem` ids then (k, E_Type v) : acc else acc) [] fields
            return (Addition t (Number 2), env' <<: matches <: ("epsilon", E_Type $ Record []))
        _ -> Left "importTypeCheck: importList: exports not a record"

stmtTypeCheck :: Stmt -> TypeEnv -> StateT Int (Either String) (SType, TypeEnv)
stmtTypeCheck (While e ss) env = do
    t1 <- exprTypeCheck e env
    (t2, env') <- stmtsTypeCheck ss env
    x <- fresh "x"
    case t1 of
        Cost t1' -> do
            let t = Addition (Multiplication x (Addition t1' t2)) t1'
            return (t, env')


stmtsTypeCheck :: [Stmt] -> TypeEnv -> StateT Int (Either String) (SType, TypeEnv)
stmtsTypeCheck [] env = return (Number 0, env)
stmtsTypeCheck (s:ss) env = do
    (t1, env') <- stmtTypeCheck s env
    (t2, env'') <- stmtsTypeCheck ss env'
    return (Addition t1 t2, env'')

exprTypeCheck :: Expr -> TypeEnv -> StateT Int (Either String) EType
exprTypeCheck (Lit (IntLit _)) env = return (Cost $ Number 0)
exprTypeCheck (Var x) env = do
    t <- lift $ envLookup x env
    case t of
        S_Type t' -> return $ Cost t'
        E_Type t' -> return t'
exprTypeCheck (BO binop e1 e2) env = do
    t1 <- exprTypeCheck e1 env
    t2 <- exprTypeCheck e2 env
    case (t1, t2) of
        (Function _ _ , _) -> lift $ Left "exprTypeCheck: binop: e1 is a function"
        (_, Function _ _) -> lift $ Left "exprTypeCheck: binop: e2 is a function"
        (Record _, _) -> lift $ Left "exprTypeCheck: binop: e1 is a record"
        (_, Record _) -> lift $ Left "exprTypeCheck: binop: e2 is a record"
        (Cost t1', Cost t2') ->
            let operCost = const $ Number 0
            in  return $ Cost $ Addition (Addition t1' t2') (operCost binop)
exprTypeCheck (Proj ident1 ident2) env = do
    t <- lift $ envLookup ident1 env
    case t of
        E_Type (Record fields) -> case lookup ident2 fields of
            Just t' -> return t'
            Nothing -> lift $ Left "exprTypeCheck: proj: field not found"
        _ -> lift $ Left "exprTypeCheck: proj: not a record"
exprTypeCheck (Lit (CompLit ids _)) env = do
    freshTypeVar <- fresh "t"
    let t = Function ids freshTypeVar
    return t
