module AbstractMachine where

import Control.Monad.State.Lazy
import Data.List (intercalate)
import Data.Map qualified as M

import AST qualified

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

data RuntimeValue
    = RVInt Int
    | RVObj (M.Map String RuntimeValue)
    | RVComp [String] [AST.Stmt] String
    deriving (Eq, Show)

localLookup ::
    String ->
    String ->
    M.Map String (M.Map String RuntimeValue) ->
    Either String RuntimeValue
localLookup scope ident env = case M.lookup scope env of
    Just m -> case M.lookup ident m of
        Just val -> Right val
        Nothing -> Left $ "Variable " ++ ident ++ " not found in scope " ++ scope
    Nothing -> Left $ "Scope " ++ scope ++ " not found"

localInsert ::
    String ->
    String ->
    RuntimeValue ->
    M.Map String (M.Map String RuntimeValue) ->
    M.Map String (M.Map String RuntimeValue)
localInsert scope ident value = M.insertWith M.union scope (M.singleton ident value)

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

run ::
    [RuntimeSyntax] ->
    [RuntimeValue] ->
    [String] ->
    StateT
        ( M.Map String AST.SrcFile
        , M.Map String (M.Map String RuntimeValue)
        , M.Map String RuntimeValue
        )
        (Either String)
        (M.Map String (M.Map String RuntimeValue))
-- end configuration
run [] _ _ = do
    (_, locals, _) <- get
    return locals

-- binoperations
run (Expr (AST.BO bo e1 e2) : stack) vs ss = run (Expr e1 : Expr e2 : BinOp bo : stack) vs ss
run (BinOp bo : stack) (RVInt n2 : RVInt n1 : vs) ss =
    run stack (RVInt (op bo) : vs) ss
    where
        op AST.Add = n1 + n2
        op AST.Sub = n1 - n2
        op AST.NotEq = if n1 /= n2 then 1 else 0

-- var
run (Expr (AST.Var v) : stack) vs (s : ss) = do
    (_, locals, _) <- get
    val <- lift $ localLookup s v locals
    run stack (val : vs) (s : ss)

-- proj
run (Expr (AST.Proj ident field) : stack) vs (s : ss) = do
    (_, locals, _) <- get
    val <- lift $ localLookup s ident locals
    case val of
        RVObj obj -> case M.lookup field obj of
            Just lit -> run stack (lit : vs) (s : ss)
            Nothing -> lift $ Left $ "Field " ++ field ++ " not found in object " ++ ident
        _ -> lift $ Left $ "Variable " ++ ident ++ " is not an object"

-- if rules
run (Stmt (AST.If cond t f) : stack) vs ss = run (Expr cond : Branch t f : stack) vs ss
run (Branch t f : stack) (RVInt n : vs) ss =
    run (if n /= 0 then map Stmt t else map Stmt f ++ stack) vs ss
-- while rules
run (Stmt (AST.While cond body) : stack) vs ss = run (Expr cond : While' cond body : stack) vs ss
run wstack@(While' _ body : stack) (RVInt n : vs) ss =
    run (if n /= 0 then map Stmt body ++ wstack else stack) vs ss
-- let and assign
run (Stmt (AST.Let ident e) : stack) vs ss = run (Expr e : Bind ident : stack) vs ss
run (Bind ident : stack) (v : vs) (s : ss) = do
    (files, locals, exports) <- get
    put (files, localInsert s ident v locals, exports)
    run stack vs (s : ss)
run (Stmt (AST.Assign ident e) : stack) vs ss = run (Expr e : Bind ident : stack) vs ss
-- imports
run (Import (AST.ImportStar ident f) : stack) vs ss = do
    (files, _, _) <- get
    src <- lift $ fileLookup f files
    run (SrcFile src : PopScope : BindExports ident : EmptyExports : stack) vs (f : ss)
run (Import (AST.ImportList idents f) : stack) vs ss = do
    (files, _, _) <- get
    src <- lift $ fileLookup f files
    run (SrcFile src : PopScope : map BindExport idents ++ EmptyExports : stack) vs (f : ss)
run (PopScope : stack) vs (_ : ss) = run stack vs ss
run (EmptyExports : stack) vs ss = do
    (files, locals, _) <- get
    put (files, locals, M.empty)
    run stack vs ss
run (BindExports ident : stack) vs (s : ss) = do
    (files, locals, exports) <- get
    put (files, localInsert s ident (RVObj exports) locals, exports)
    run stack vs (s : ss)
run (BindExport ident : stack) vs (s : ss) = do
    (files, locals, exports) <- get
    val <- lift $ exportLookup ident exports
    put (files, localInsert s ident val locals, exports)
    run stack vs (s : ss)

-- export
run (Export (AST.Export ident) : stack) vs (s : ss) = do
    (files, locals, exports) <- get
    val <- lift $ localLookup s ident locals
    put (files, locals, M.insert ident val exports)
    run stack vs (s : ss)

-- literals
run (Lit (AST.IntLit n) : stack) vs ss = run stack (RVInt n : vs) ss
run (Lit (AST.CompLit idents body) : stack) vs (s : ss) = run stack (RVComp idents body s : vs) (s : ss)
-- compcall
run (Stmt (AST.CompCall comp args) : stack) vs ss = run (Expr comp : CompCall' args : stack) vs ss
run (CompCall' args : stack) (RVComp idents body s : vs) ss =
    run (lets ++ stmts ++ [PopScope] ++ stack) vs (s : ss)
    where
        lets = zipWith (\i a -> Stmt (AST.Let i a)) idents args
        stmts = map Stmt body

-- failure
run stack vs ss = do
    (files, locals, exports) <- get
    let stringFiles = show $ M.toList files
    let stringLocals = show $ M.toList locals
    let stringExports = show $ M.toList exports
    lift $ Left $ intercalate "\n" [show stack, show vs, show ss, stringFiles, stringLocals, stringExports]
