module TypeChecker where

import AST
import RecursionSchemes

import Data.Map qualified as M
import Control.Monad.State.Lazy

data AbstractType = T | AbstractType :-> AbstractType

data SomeType = SomeType

data Constraint = Less String String

instance Show Constraint where
    show (Less a b) = a ++ " subset.eq.sq " ++ b

unsafeLookup :: Ord k => k -> M.Map k v -> v
unsafeLookup k m = case M.lookup k m of
  Just v -> v
  Nothing -> error "unsafeLookup: key not found"

-- | TODO should probably make a safe version of this
unpackSrcFile :: M.Map String (SrcFile String) -> String -> Term SrcFile
unpackSrcFile srcFiles name = In $ SrcFile is' ss es where
    SrcFile is ss es = unsafeLookup name srcFiles
    is' = map (fmap (unpackSrcFile srcFiles)) is

srcFileConstraints :: Term SrcFile -> State (M.Map String SomeType) [Constraint]
srcFileConstraints = cata f where
    f :: SrcFile (State (M.Map String SomeType) [Constraint]) -> State (M.Map String SomeType) [Constraint]
    f (SrcFile is ss es) = do
        c1 <- mapM importConstraints is
        c2 <- mapM stmtConstraints ss
        c3 <- mapM exportConstraints es
        return $ concat c1 ++ concat c2 ++ concat c3

importConstraints :: Import (State (M.Map String SomeType) [Constraint]) -> State (M.Map String SomeType) [Constraint]
importConstraints (ImportStar _ s) = s
importConstraints (ImportList _ s) = s

stmtConstraints :: Stmt -> State (M.Map String SomeType) [Constraint]
stmtConstraints (If e s1 s2) = do
    c1 <- exprConstraints e
    c2 <- mapM stmtConstraints s1
    c3 <- mapM stmtConstraints s2
    return $ c1 ++ concat c2 ++ concat c3
stmtConstraints (While e s) = do
    c1 <- exprConstraints e
    c2 <- mapM stmtConstraints s
    return $ c1 ++ concat c2
stmtConstraints (Assign _ e) = exprConstraints e
stmtConstraints (Let _ e) = exprConstraints e
stmtConstraints (CompCall e es) = do
    c1 <- exprConstraints e
    c2 <- mapM exprConstraints es
    return $ c1 ++ concat c2 ++ compCallConstraint e es

compCallConstraint :: Expr -> [Expr] -> [Constraint]
compCallConstraint e es =
    let f (i, e') = Less ("(" ++ e' ++ ")_" ++ show i)
    in  zipWith f (map (,show e) ([1..] :: [Int])) (map show es)

exprConstraints :: Expr -> State (M.Map String SomeType) [Constraint]
exprConstraints _ = return []

exportConstraints :: Export -> State (M.Map String SomeType) [Constraint]
exportConstraints (Export _) = return []
