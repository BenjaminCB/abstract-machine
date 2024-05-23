module ConstraintGeneration where

import AST
import RecursionSchemes

import Data.Map qualified as M
import Data.List (intercalate)
import Control.Monad.State.Lazy

data AbstractType = T | AbstractType :-> AbstractType

data SomeType = SomeType

data Constraint = Less String String
                | Type String [String]
                | InTypes String [String]
                | NotInTypes String [String]
                | Env String String
                | InEnv String
                deriving (Eq)

instance Show Constraint where
    show (Less a b) = a ++ " subset.eq.sq " ++ b
    show (Type a bs) = a ++ ": " ++ intercalate " -> " bs
    show (InTypes a bs) = a ++ " in " ++ show bs
    show (NotInTypes a bs) = a ++ " in.not " ++ show bs
    show (Env a b) = "Gamma(" ++ a ++ ") = " ++ b
    show (InEnv a) = a ++ " in Dom(Gamma)"

type ConstraintState a = State (M.Map String SomeType, Int) a

fresh :: ConstraintState String
fresh = do
    (m, s) <- get
    put (m, s + 1)
    return $ "t_" ++ show s

freshN :: Int -> ConstraintState [String]
freshN 0 = return []
freshN n = do
    s <- fresh
    ss <- freshN (n - 1)
    return $ s : ss

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = case M.lookup k m of
  Just v -> v
  Nothing -> error "unsafeLookup: key not found"

-- | TODO should probably make a safe version of this
unpackSrcFile :: M.Map String (SrcFile String) -> String -> Term SrcFile
unpackSrcFile srcFiles name = In $ SrcFile is' ss es where
    SrcFile is ss es = unsafeLookup name srcFiles
    is' = map (fmap (unpackSrcFile srcFiles)) is

srcFileConstraints :: Term SrcFile -> ConstraintState [Constraint]
srcFileConstraints = cata f where
    f (SrcFile is ss es) = do
        c1 <- mapM importConstraints is
        c2 <- mapM stmtConstraints ss
        c3 <- mapM exportConstraints es
        return $ concat c1 ++ concat c2 ++ concat c3

importConstraints :: Import (ConstraintState [Constraint]) -> ConstraintState [Constraint]
importConstraints (ImportStar _ s) = s
importConstraints (ImportList _ s) = s

stmtConstraints :: Stmt -> ConstraintState [Constraint]
stmtConstraints (If e s1 s2) = do
    c1 <- exprConstraints e
    c2 <- mapM stmtConstraints s1
    c3 <- mapM stmtConstraints s2
    return $ c1 ++ concat c2 ++ concat c3
stmtConstraints (While e s) = do
    c1 <- exprConstraints e
    c2 <- mapM stmtConstraints s
    return $ c1 ++ concat c2
stmtConstraints (For _ _ _ s) = do
    c3 <- mapM stmtConstraints s
    return $ concat c3
stmtConstraints (Assign x e) = do
    c1 <- exprConstraints e
    return $ c1 ++ [InEnv x]
stmtConstraints (Let _ e) = exprConstraints e
stmtConstraints (CompCall e es) = do
    c1 <- exprConstraints e
    c2 <- mapM exprConstraints es
    c3 <- compCallConstraint e es
    return $ c1 ++ concat c2 ++ c3

compCallConstraint :: Expr -> [Expr] -> ConstraintState [Constraint]
compCallConstraint e es = do
    params <- freshN (length es)
    args <- freshN (length es)
    let c1 = Type (show e) params
    let c2 = zipWith Type (map show es) (map (:[]) args)
    let c3 = zipWith Less args params
    c4 <- exprConstraints e
    c5 <- mapM exprConstraints es
    return $ c1 : c2 ++ c3 ++ c4 ++ concat c5

exprConstraints :: Expr -> ConstraintState [Constraint]
exprConstraints (Var x) = do
    t <- fresh
    return [Env x t]
exprConstraints _ = return []

exportConstraints :: Export -> ConstraintState [Constraint]
exportConstraints (Export _) = return []
