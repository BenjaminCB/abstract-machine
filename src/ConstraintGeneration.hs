module ConstraintGeneration where

import AST
import RecursionSchemes
import TypeChecker

import Data.Map qualified as M
import Control.Monad.State.Lazy

type ConstraintState = StateT (M.Map String EType) (Either String) [Constraint]

data Constraint = Lub String String
    deriving (Eq)

instance Show Constraint where
    show (Lub t1 t2) = t1 ++ " <= " ++ t2

srcFileConstraints :: Term SrcFile -> ConstraintState
srcFileConstraints = cata f where
    f (SrcFile is ss es) = do
        c1 <- mapM importConstraints is
        c2 <- mapM stmtConstraints ss
        c3 <- mapM exportConstraints es
        return $ concat c1 ++ concat c2 ++ concat c3

importConstraints :: Import ConstraintState -> ConstraintState
importConstraints (ImportStar ident s) = do
    env <- get
    exports <- lift $ envLookup "epsilon" env
    put $ M.singleton "epsilon" (Record [])
    constraints <- s
    put $ env <: (ident, exports) <: ("epsilon", Record [])
    return constraints
importConstraints (ImportList ids s) = do
    env <- get
    put $ M.singleton "epsilon" (Record [])
    constraints <- s
    env' <- get
    exports <- lift $ envLookup "epsilon" env'
    case exports of
        Record fields -> do
            -- TODO does not take into account if some id is not in exports
            let matches = foldr (\(k, v) acc -> if k `elem` ids then (k, v) : acc else acc) [] fields
            put $ env <<: matches <: ("epsilon", Record [])
            return constraints
        _ -> lift $ Left "importTypeCheck: importList: exports not a record"

stmtConstraints :: Stmt -> ConstraintState
stmtConstraints (If e s1 s2) = do
    c1 <- exprConstraints e
    c2 <- mapM stmtConstraints s1
    c3 <- mapM stmtConstraints s2
    return $ c1 ++ concat c2 ++ concat c3
stmtConstraints (While e s) = do
    c1 <- exprConstraints e
    c2 <- mapM stmtConstraints s
    return $ c1 ++ concat c2
stmtConstraints (For ident _ _ s) = do
    env <- get
    put $ env <: (ident, Cost $ Number 0)
    c3 <- mapM stmtConstraints s
    return $ concat c3
stmtConstraints (Assign x e) = do
    env <- get
    t <- lift $ evalStateT (exprTypeCheck e (E_Type <$> env)) 0
    cs <- exprConstraints e
    put $ env <: (x, t)
    return cs
stmtConstraints (Let x e) = do
    env <- get
    t <- lift $ evalStateT (exprTypeCheck e (E_Type <$> env)) 0
    cs <- exprConstraints e
    put $ env <: (x, t)
    return cs
stmtConstraints (CompCall e es) = do
    env <- get
    t <- lift $ evalStateT (exprTypeCheck e (E_Type <$> env)) 0
    ts <- lift $ evalStateT (exprsTypeCheck es (E_Type <$> env)) 0
    case t of
        Function params _ -> do
            lift $ sat (length params == length ts) "stmtConstraints: compCall: wrong number of arguments"
            let cs = zipWith Lub (map show ts) params
            return cs
        _ -> lift $ Left "stmtConstraints: compCall: not a function"

stmtsConstraints :: [Stmt] -> ConstraintState
stmtsConstraints [] = return []
stmtsConstraints (s:ss) = do
    c1 <- stmtConstraints s
    c2 <- stmtsConstraints ss
    return $ c1 ++ c2

exprConstraints :: Expr -> ConstraintState
exprConstraints (Lit (CompLit params body)) = do
    env <- get
    put $ env <<: zip params (map (Cost . Variable) params)
    stmtsConstraints body
exprConstraints _ = return []

exportConstraints :: Export -> ConstraintState
exportConstraints (Export ident) = do
    env <- get
    exports <- lift $ envLookup "epsilon" env
    t <- lift $ envLookup ident env
    case exports of
        Record fields -> do
            let exports' = Record $ (ident, t) : fields
            put $ env <: ("epsilon", exports')
            return []
        _ -> lift $ Left "exportConstraints: exports not a record"
