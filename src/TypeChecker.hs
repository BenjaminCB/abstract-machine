module TypeChecker where

import AST
import RecursionSchemes
import Auxiliary

import Data.Map qualified as M
import Control.Monad.State.Lazy
import Data.List (intercalate)

data Type = S_Type SType
          | E_Type EType
          deriving (Show, Eq)

data SType = Number Int
           | Variable String
           | Addition SType SType
           | Multiplication SType SType
           | Maximum SType SType
           deriving (Eq)

instance Show SType where
    show (Number n) = show n
    show (Variable x) = x
    show (Addition t1 t2) = "(" ++ show t1 ++ " + " ++ show t2 ++ ")"
    show (Multiplication t1 t2) = "(" ++ show t1 ++ " * " ++ show t2 ++ ")"
    show (Maximum t1 t2) = "(" ++ show t1 ++ " arrow.t " ++ show t2 ++ ")"

sTypeRewriteRules :: SType -> SType
sTypeRewriteRules (Addition (Number 0) t) = sTypeRewriteRules t
sTypeRewriteRules (Addition t (Number 0)) = sTypeRewriteRules t
sTypeRewriteRules (Multiplication (Number 0) _) = Number 0
sTypeRewriteRules (Multiplication _ (Number 0)) = Number 0
sTypeRewriteRules (Multiplication (Number 1) t) = sTypeRewriteRules t
sTypeRewriteRules (Multiplication t (Number 1)) = sTypeRewriteRules t
sTypeRewriteRules (Maximum (Number 0) t) = sTypeRewriteRules t
sTypeRewriteRules (Maximum t (Number 0)) = sTypeRewriteRules t
sTypeRewriteRules (Addition (Number n1) (Number n2)) = Number (n1 + n2)
sTypeRewriteRules (Multiplication (Number n1) (Number n2)) = Number (n1 * n2)
sTypeRewriteRules (Maximum (Number n1) (Number n2)) = Number (max n1 n2)
sTypeRewriteRules (Number n) = Number n
sTypeRewriteRules (Variable x) = Variable x
sTypeRewriteRules (Addition t1 t2) = Addition (sTypeRewriteRules t1) (sTypeRewriteRules t2)
sTypeRewriteRules (Multiplication t1 t2) = Multiplication (sTypeRewriteRules t1) (sTypeRewriteRules t2)
sTypeRewriteRules (Maximum t1 t2) = Maximum (sTypeRewriteRules t1) (sTypeRewriteRules t2)

data EType = Cost SType
           | Function [String] SType
           | Record [(String, EType)]
           deriving (Eq)

instance Show EType where
    show (Cost t) = show t
    show (Function ids t) = "(" ++ intercalate " -> " ids ++ " -> " ++ show t ++ ")"
    show (Record fields) = "({" ++ intercalate ", " (map (\(k, v) -> k ++ ": " ++ show v) fields) ++ "})"

toNumber :: EType -> SType
toNumber (Cost t) = t
toNumber _ = Number 0

type TypeEnv = M.Map String Type

instance LUB Type where
    (\/) = undefined

instance LUB SType where
    Addition t1 t2 \/ Addition t3 t4 = Addition (t1 \/ t3) (t2 \/ t4)
    Multiplication t1 t2 \/ Multiplication t3 t4 = Multiplication (t1 \/ t3) (t2 \/ t4)
    Number n1 \/ Number n2 = Number (max n1 n2)
    Variable x1 \/ Variable x2 = Maximum (Variable x1) (Variable x2)
    Maximum t1 t2 \/ Maximum t3 t4 = Maximum (Maximum t1 t3) (Maximum t2 t4)
    _ \/ _ = error "instance LUB SType: not implemented"

instance LUB EType where
    Cost t1 \/ Cost t2 = Cost (t1 \/ t2)
    Function _ _ \/ Function _ _ = error "instance LUB EType: function: not implemented"
    -- Function ids1 t1 \/ Function ids2 t2
    --     | ids1 == ids2 = Function ids1 (t1 \/ t2)
    --     | otherwise    = error "instance LUB EType: function: different ids"
    Record fields1 \/ Record fields2
        | map fst fields1 == map fst fields2 =
            Record $ zipWith (\(k, v1) (_, v2) -> (k, v1 \/ v2)) fields1 fields2
        | otherwise = error "instance LUB EType: record: different fields"
    _ \/ _ = error "instance LUB EType: not implemented"


instance LUB (M.Map String Type) where
    (\/) = M.unionWith (\/)

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

sat :: Bool -> String -> Either String ()
sat True _ = Right ()
sat False msg = Left msg

envLookup :: (Ord k, Show k) => k -> M.Map k v -> Either String v
envLookup k env = case M.lookup k env of
    Just v -> Right v
    Nothing -> Left $ "envLookup: key not found: " ++ show k

srcFileTypeCheck :: Term SrcFile -> TypeEnv -> Either String (SType, TypeEnv)
srcFileTypeCheck srcFile env = cata f srcFile env where
    f (SrcFile is ss es) _ = do
        (t1, env') <- importsTypeCheck is env
        (t2, env'') <- evalStateT (stmtsTypeCheck ss env') 0
        (t3, env''') <- exportsTypeCheck es env''
        return (Addition (Addition t1 t2) t3, env''')

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
            let t' = Addition (Number $ length matches) $ Addition (Number 2) t
            return (t', env' <<: matches <: ("epsilon", E_Type $ Record []))
        _ -> Left "importTypeCheck: importList: exports not a record"

importsTypeCheck :: [Import (TypeEnv -> Either String (SType, TypeEnv))] -> TypeEnv -> Either String (SType, TypeEnv)
importsTypeCheck [] env = Right (Number 0, env)
importsTypeCheck (i:is) env = do
    (t1, env') <- importTypeCheck i env
    (t2, env'') <- importsTypeCheck is env'
    return (Addition t1 t2, env'')

stmtTypeCheck :: Stmt -> TypeEnv -> StateT Int (Either String) (SType, TypeEnv)
stmtTypeCheck (While e ss) env = do
    t1 <- exprTypeCheck e env
    (t2, env') <- stmtsTypeCheck ss env
    x <- fresh "x"
    case t1 of
        Cost t1' -> do
            let t = Addition (Multiplication (Variable x) (Addition t1' t2)) t1'
            return (t, env')
        _ -> lift $ Left "stmtTypeCheck: while: not a cost"
stmtTypeCheck (For ident n m ss) env = do
    lift $ sat (n <= m) "stmtTypeCheck: for: n > m"
    (t1, env') <- stmtsTypeCheck ss (env <: (ident, S_Type $ Number 0))
    let diff = Number $ m - n + 1
    let t = Addition (Multiplication diff t1) diff
    return (t, env')
stmtTypeCheck (CompCall comp args) env = do
    t <- exprTypeCheck comp env
    case t of
        Function params t' -> do
            lift $ sat (length params == length args) "stmtTypeCheck: compCall: wrong number of arguments"
            ta <- exprsTypeCheck args env
            let t'' = foldr1 Addition $ map toNumber ta ++ [t'] ++ replicate (length ta) (Number 1)
            return (t'', env)
        _ -> lift $ Left "stmtTypeCheck: compCall: not a function"

stmtTypeCheck (If e ss1 ss2) env = do
    t0 <- exprTypeCheck e env
    (t1, env') <- stmtsTypeCheck ss1 env
    (t2, env'') <- stmtsTypeCheck ss2 env
    case t0 of
        Cost t -> return (Addition (Maximum t1 t2) t, env' \/ env'')
        _ -> lift $ Left "stmtTypeCheck: if: not a cost"
stmtTypeCheck (Let x e) env = do
    t <- exprTypeCheck e env
    case t of
        Cost t' -> return (Addition t' (Number 1), env <: (x, S_Type $ Number 0))
        Function _ _ -> return (Number 1, env <: (x, E_Type t))
        Record _ -> return (Number 1, env <: (x, E_Type t))
stmtTypeCheck (Assign x e) env = do
    lift $ sat (M.member x env) "stmtTypeCheck: assign: x not in env"
    t <- exprTypeCheck e env
    case t of
        Cost t' -> return (Addition t' (Number 1), env <: (x, S_Type $ Number 0))
        Function _ _ -> return (Number 1, env <: (x, E_Type t))
        Record _ -> return (Number 1, env <: (x, E_Type t))

stmtsTypeCheck :: [Stmt] -> TypeEnv -> StateT Int (Either String) (SType, TypeEnv)
stmtsTypeCheck [] env = return (Number 0, env)
stmtsTypeCheck (s:ss) env = do
    (t1, env') <- stmtTypeCheck s env
    (t2, env'') <- stmtsTypeCheck ss env'
    return (Addition t1 t2, env'')

exportTypeCheck :: Export -> TypeEnv -> Either String (SType, TypeEnv)
exportTypeCheck (Export ident) env = do
    t <- envLookup ident env
    exports <- envLookup "epsilon" env
    case (exports, t) of
        (E_Type (Record fields), E_Type t') -> do
            let exports' = E_Type $ Record $ (ident, t') : fields
            return (Number 1, env <: ("epsilon", exports'))
        (E_Type (Record fields), S_Type t') -> do
            let exports' = E_Type $ Record $ (ident, Cost t') : fields
            return (Number 1, env <: ("epsilon", exports'))
        _ -> Left "exportTypeCheck: not a record"

exportsTypeCheck :: [Export] -> TypeEnv -> Either String (SType, TypeEnv)
exportsTypeCheck [] env = Right (Number 0, env)
exportsTypeCheck (e:es) env = do
    (t1, env') <- exportTypeCheck e env
    (t2, env'') <- exportsTypeCheck es env'
    return (Addition t1 t2, env'')

exprTypeCheck :: Expr -> TypeEnv -> StateT Int (Either String) EType
exprTypeCheck (Lit (IntLit _)) _ = return (Cost $ Number 0)
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
exprTypeCheck (Lit (CompLit ids body)) env = do
    (ta, _) <- stmtsTypeCheck body (env <<: zip ids (map (E_Type . Cost . Variable) ids))
    let t = Function ids ta
    return t

exprsTypeCheck :: [Expr] -> TypeEnv -> StateT Int (Either String) [EType]
exprsTypeCheck [] _ = return []
exprsTypeCheck (e:es) env = do
    t1 <- exprTypeCheck e env
    t2 <- exprsTypeCheck es env
    return $ t1 : t2
