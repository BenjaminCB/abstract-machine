module AbstractMachine where

import Control.Monad.State.Lazy
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
    deriving (Eq, Show)

data RuntimeValue
    = RVLit AST.Lit
    | SomeOtherStuff
    deriving (Eq, Show)

localLookup ::
    String ->
    String ->
    M.Map String (M.Map String AST.Lit) ->
    Either String AST.Lit
localLookup s v env = case M.lookup s env of
    Just m -> case M.lookup v m of
        Just lit -> Right lit
        Nothing -> Left $ "Variable " ++ v ++ " not found in scope " ++ s
    Nothing -> Left $ "Scope " ++ s ++ " not found"

run ::
    [RuntimeSyntax] ->
    [RuntimeValue] ->
    [String] ->
    StateT
        ( M.Map String AST.SrcFile
        , M.Map String (M.Map String AST.Lit)
        , M.Map String AST.Lit
        )
        (Either String)
        (M.Map String (M.Map String AST.Lit))
-- modify to be strictly numlit
run (Lit lit : stack) vs ss = run stack (RVLit lit : vs) ss
-- binoperations
run (Expr (AST.BO bo e1 e2) : stack) vs ss = run (Expr e1 : Expr e2 : BinOp bo : stack) vs ss
run (BinOp bo : stack) (RVLit (AST.IntLit n2) : RVLit (AST.IntLit n1) : vs) ss =
    run stack (RVLit (AST.IntLit $ op bo) : vs) ss
    where
        op AST.Add = n1 + n2
        op AST.Sub = n1 - n2
        op AST.NotEq = if n1 /= n2 then 1 else 0
-- var
run (Expr (AST.Var v) : stack) vs (s : ss) = do
    (_, locals, _) <- get
    val <- lift $ localLookup s v locals
    run stack (RVLit val : vs) (s : ss)
-- proj
run (Expr (AST.Proj ident field) : stack) vs (s : ss) = do
    (_, locals, _) <- get
    val <- lift $ localLookup s ident locals
    case val of
        AST.Obj obj -> case M.lookup field obj of
            Just lit -> run stack (RVLit lit : vs) (s : ss)
            Nothing -> lift $ Left $ "Field " ++ field ++ " not found in object " ++ ident
        _ -> lift $ Left $ "Variable " ++ ident ++ " is not an object"
-- if rules
run (Stmt (AST.If cond t f) : stack) vs ss = run (Expr cond : Branch t f : stack) vs ss
run (Branch t f : stack) (RVLit (AST.IntLit n) : vs) ss =
    run (if n /= 0 then map Stmt t else map Stmt f ++ stack) vs ss
-- while rules
run (Stmt (AST.While cond body) : stack) vs ss = run (Expr cond : While' cond body : stack) vs ss
run wstack@(While' cond body : stack) (RVLit (AST.IntLit n) : vs) ss =
    run (if n /= 0 then map Stmt body ++ wstack else stack) vs ss
-- let1
-- let2
-- assign
-- import1
-- import2
-- import3
-- import4
-- export
-- compdef
-- compcall1
-- compcall2
-- compcall3
run _ _ _ = lift $ Left "Unimplemented case"
