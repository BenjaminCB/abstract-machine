module AbstractMachine where

import Control.Monad.State.Lazy
import Data.Map qualified as M
import Data.List (intercalate)
import Data.Bifunctor (first, second)

import AST qualified
import Auxiliary

data RuntimeSyntax
    = SrcFile AST.SrcFile
    | Import AST.Import
    | Export AST.Export
    | Stmt AST.Stmt
    | Expr AST.Expr
    | BinOp AST.BinOp
    | Lit AST.Lit
    | Branch [AST.Stmt] [AST.Stmt]
    | While' AST.Expr [AST.Stmt]
    | CompCall' [AST.Expr]
    | Bind String
    | BindExport String
    | BindExports String
    | EmptyExports
    | PopScope
    deriving (Eq, Show)

showSyntaxConstructor :: RuntimeSyntax -> String
showSyntaxConstructor (SrcFile _) = "SrcFile"
showSyntaxConstructor (Import _) = "Import"
showSyntaxConstructor (Export _) = "Export"
showSyntaxConstructor (Stmt _) = "Stmt"
showSyntaxConstructor (Expr _) = "Expr"
showSyntaxConstructor (BinOp _) = "BinOp"
showSyntaxConstructor (Lit _) = "Lit"
showSyntaxConstructor (Branch _ _) = "Branch"
showSyntaxConstructor (While' _ _) = "While'"
showSyntaxConstructor (CompCall' _) = "CompCall'"
showSyntaxConstructor (Bind _) = "Bind"
showSyntaxConstructor (BindExport _) = "BindExport"
showSyntaxConstructor (BindExports _) = "BindExports"
showSyntaxConstructor EmptyExports = "EmptyExports"
showSyntaxConstructor PopScope = "PopScope"

data RuntimeValue
    = RVInt Int
    | RVObj (M.Map String RuntimeValue)
    | RVComp [String] [AST.Stmt] String
    deriving (Eq, Show)

data AMState = AMState [RuntimeSyntax] (M.Map String AST.SrcFile) (M.Map (String, String) RuntimeValue) (M.Map String RuntimeValue) [RuntimeValue] [String]
    deriving (Eq, Show)

type Trace = [Either (AMState, String) AMState]

traceToTuple :: Trace -> (AMState, [String], AMState)
traceToTuple [] = error "Empty trace"
traceToTuple (Right st : _) = (st, [], st)
traceToTuple (Left (st, rule) : xs) = traceToTuple' (st, [rule], st) xs
    where
        traceToTuple' t [] = t
        traceToTuple' (start, rules, _) (Right end : _) = (start, rules, end)
        traceToTuple' (start, rules, _) (Left (end, rule') : xs') = traceToTuple' (start, rules ++ [rule'], end) xs'

runtimeValueToTypst :: RuntimeValue -> String
runtimeValueToTypst (RVInt n) = show n
runtimeValueToTypst (RVObj obj) = concat
    [ "{ ", intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ runtimeValueToTypst v) (M.toList obj)), " }" ]
runtimeValueToTypst (RVComp idents body s) = concat
    [ "(", "(", intercalate "," (map (\i -> "\"" ++ i ++ "\"") idents), ")", ", ", show body, ", \"", s, "\")" ]

localsToTypst :: M.Map (String, String) RuntimeValue -> String
localsToTypst = unlines
              . map (\((s, i), v) -> "(\"" ++ s ++ "\",\"" ++ i ++ "\") -> " ++ runtimeValueToTypst v ++ "\\")
              . M.toList

traceToTypst :: Trace -> String
traceToTypst = tupleToTypst . traceToTuple
    where
        tupleToTypst (start, rules, end) = unlines
            [ amStateToTypst start ++ "\\"
            , unlines (appendAtEveryN 9 '\\' (map ruleToTypst rules)) ++ "\\"
            , amStateToTypst end ++ "\\" ]
        amStateToTypst (AMState _ _ locals _ _ _) = localsToTypst locals
        ruleToTypst r = "tick(" ++ r ++ ")"

showValueConstructor :: RuntimeValue -> String
showValueConstructor (RVInt _) = "RVInt"
showValueConstructor (RVObj _) = "RVObj"
showValueConstructor (RVComp {}) = "RVComp"

localLookup ::
    String ->
    String ->
    M.Map (String, String) RuntimeValue ->
    Either String RuntimeValue
localLookup scope ident env = case M.lookup (scope, ident) env of
    Just val -> Right val
    Nothing -> Left $ "Variable " ++ ident ++ " not found in scope " ++ scope

localInsert ::
    String ->
    String ->
    RuntimeValue ->
    M.Map (String, String) RuntimeValue ->
    M.Map (String, String) RuntimeValue
localInsert scope ident = M.insert (scope, ident)

exportLookup ::
    String ->
    M.Map String RuntimeValue ->
    Either String RuntimeValue
exportLookup v exports = case M.lookup v exports of
    Just lit -> Right lit
    Nothing -> Left $ "Export " ++ v ++ " not found"

fileLookup ::
    String ->
    M.Map String AST.SrcFile ->
    Either String AST.SrcFile
fileLookup f files = case M.lookup f files of
    Just src -> Right src
    Nothing -> Left $ "File " ++ f ++ " not found"

pushTrace ::
    AMState ->
    String ->
    StateT (Int, Trace) (Either String) ()
pushTrace st rule = do
    modify (second (++[Left(st, rule)]))
    return ()

addCost :: (Int -> Int) -> StateT (Int, Trace) (Either String) ()
addCost f = modify (first f)

runFile ::
    String ->
    AST.SrcFile ->
    M.Map String AST.SrcFile ->
    Either String (M.Map (String, String) RuntimeValue)
runFile entryPoint src fileGetter =
    evalStateT (run (AMState [SrcFile src] fileGetter M.empty M.empty [] [entryPoint])) (0, [])

runFileWithState ::
    String ->
    AST.SrcFile ->
    M.Map String AST.SrcFile ->
    Either String (M.Map (String, String) RuntimeValue, (Int, Trace))
runFileWithState entryPoint src fileGetter =
    runStateT (run (AMState [SrcFile src] fileGetter M.empty M.empty [] [entryPoint])) (0, [])

run :: AMState -> StateT (Int, Trace) (Either String) (M.Map (String, String) RuntimeValue)
-- end configuration
run st@(AMState [] _ locals _ _ _) = do
    modify (second (++[Right st]))
    return locals

-- srcfile
run st@(AMState (SrcFile (AST.SrcFile imports stmts exports) : stack) fg ls es vs ss) = do
    pushTrace st "SrcFile"
    run (AMState (map Import imports ++ map Stmt stmts ++ map Export exports ++ stack) fg ls es vs ss)

-- binoperations
run st@(AMState (Expr (AST.BO bo e1 e2) : stack) fg ls es vs ss) = do
    pushTrace st "BinOp1"
    run (AMState (Expr e1 : Expr e2 : BinOp bo : stack) fg ls es vs ss)
run st@(AMState (BinOp bo : stack) fg ls es (RVInt n2 : RVInt n1 : vs) ss) = do
    pushTrace st "BinOp2"
    run (AMState stack fg ls es (RVInt (op bo) : vs) ss)
    where
        op AST.Add = n1 + n2
        op AST.Sub = n1 - n2
        op AST.NotEq = if n1 /= n2 then 1 else 0

-- var
run st@(AMState (Expr (AST.Var v) : stack) fg ls es vs (s : ss)) = do
    pushTrace st "Var"
    val <- lift $ localLookup s v ls
    run (AMState stack fg ls es (val : vs) (s : ss))

-- proj
run st@(AMState (Expr (AST.Proj ident field) : stack) fg ls es vs (s : ss)) = do
    pushTrace st "Proj"
    val <- lift $ localLookup s ident ls
    case val of
        RVObj obj -> case M.lookup field obj of
            Just lit -> run (AMState stack fg ls es (lit : vs) (s : ss))
            Nothing -> lift $ Left $ "Field " ++ field ++ " not found in object " ++ ident ++ " in scope " ++ s
        _ -> lift $ Left $ "Variable " ++ ident ++ " is not an object"

-- if rules
run st@(AMState (Stmt (AST.If cond t f) : stack) fg ls es vs ss) = do
    pushTrace st "If"
    run (AMState (Expr cond : Branch t f : stack) fg ls es vs ss)
run st@(AMState (Branch t f : stack) fg ls es (RVInt n : vs) ss) = do
    pushTrace st "Branch"
    run (AMState (if n /= 0 then map Stmt t else map Stmt f ++ stack) fg ls es vs ss)

-- while rules
run st@(AMState (Stmt (AST.While cond body) : stack) fg ls es vs ss) = do
    pushTrace st "While"
    run (AMState (Expr cond : While' cond body : stack) fg ls es vs ss)
run st@(AMState wstack@(While' cond body : stack) fg ls es (RVInt n : vs) ss) = do
    pushTrace st "While'"
    run (AMState (if n /= 0 then map Stmt body ++ [Expr cond] ++ wstack else stack) fg ls es vs ss)

-- let and assign
run st@(AMState (Stmt (AST.Let ident e) : stack) fg ls es vs ss) = do
    pushTrace st "Let"
    run (AMState (Expr e : Bind ident : stack) fg ls es vs ss)
run st@(AMState (Bind ident : stack) fg ls es (v : vs) (s : ss)) = do
    pushTrace st "Bind"
    addCost (+1)
    run (AMState stack fg (localInsert s ident v ls) es vs (s : ss))
run st@(AMState (Stmt (AST.Assign ident e) : stack) fg ls es vs ss) = do
    pushTrace st "Assign"
    run (AMState (Expr e : Bind ident : stack) fg ls es vs ss)

-- imports
run st@(AMState (Import (AST.ImportStar ident f) : stack) fg ls es vs ss) = do
    pushTrace st "Import1"
    addCost (+2)
    src <- lift $ fileLookup f fg
    run (AMState (SrcFile src : PopScope : BindExports ident : EmptyExports : stack) fg ls es vs (f : ss))
run st@(AMState (Import (AST.ImportList idents f) : stack) fg ls es vs ss) = do
    pushTrace st "Import2"
    addCost (+2)
    src <- lift $ fileLookup f fg
    run (AMState (SrcFile src : PopScope : map BindExport idents ++ EmptyExports : stack) fg ls es vs (f : ss))
run st@(AMState (PopScope : stack) fg ls es vs (_ : ss)) = do
    pushTrace st "PopScope"
    run (AMState stack fg ls es vs ss)
run st@(AMState (EmptyExports : stack) fg ls _ vs ss) = do
    pushTrace st "EmptyExports"
    run (AMState stack fg ls M.empty vs ss)
run st@(AMState (BindExports ident : stack) fg ls es vs (s : ss)) = do
    pushTrace st "BindAll"
    addCost (+1)
    run (AMState stack fg (localInsert s ident (RVObj es) ls) es vs (s : ss))
run st@(AMState (BindExport ident : stack) fg ls es vs (s : ss)) = do
    pushTrace st "BindSelected"
    addCost (+1)
    val <- lift $ exportLookup ident es
    run (AMState stack fg (localInsert s ident val ls) es vs (s : ss))

-- export
run st@(AMState (Export (AST.Export ident) : stack) fg ls es vs (s : ss)) = do
    pushTrace st "Export"
    addCost (+1)
    val <- lift $ localLookup s ident ls
    run (AMState stack fg ls (M.insert ident val es) vs (s : ss))

-- literals
run st@(AMState (Expr (AST.Lit lit) : stack) fg ls es vs ss) = do
    pushTrace st "ExprLit"
    run (AMState (Lit lit : stack) fg ls es vs ss)
run st@(AMState (Lit (AST.IntLit n) : stack) fg ls es vs ss) = do
    pushTrace st "Int"
    run (AMState stack fg ls es (RVInt n : vs) ss)
run st@(AMState (Lit (AST.CompLit idents body) : stack) fg ls es vs (s : ss)) = do
    pushTrace st "Comp"
    run (AMState stack fg ls es (RVComp idents body s : vs) (s : ss))

-- compcall
run st@(AMState (Stmt (AST.CompCall comp args) : stack) fg ls es vs ss) = do
    pushTrace st "CompCall"
    run (AMState (Expr comp : CompCall' args : stack) fg ls es vs ss)
run st@(AMState (CompCall' args : stack) fg ls es (RVComp idents body s : vs) ss) = do
    pushTrace st "CompCall'"
    run (AMState (lets ++ stmts ++ [PopScope] ++ stack) fg ls es vs (s : ss))
    where
        lets = zipWith (\i a -> Stmt (AST.Let i a)) idents args
        stmts = map Stmt body

-- failure
run (AMState {}) = do
    trace <- get
    lift $ Left $ "Failure: " ++ show trace
