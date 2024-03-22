module AbstractMachine where

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
    | StuffToCome
    deriving (Eq, Show)

data RuntimeValue
    = RVLit AST.Lit
    | SomeOtherStuffToCome
    deriving (Eq, Show)

runMachine ::
    [RuntimeSyntax] ->
    M.Map String AST.SrcFile ->
    M.Map String (M.Map String AST.Lit) ->
    M.Map String AST.Lit ->
    [RuntimeValue] ->
    [String] ->
    Either String (M.Map String (M.Map String AST.Lit))
runMachine stack fg ls es vs ss = case (stack, vs, ss) of
    (Lit (AST.IntLit n) : stack', _, _) -> runMachine stack' fg ls es (RVLit (AST.IntLit n) : vs) ss
    (Expr (AST.BO bo e1 e2) : stack', _, _) -> runMachine (Expr e1 : Expr e2 : BinOp bo : stack') fg ls es vs ss
    (BinOp bo : stack', RVLit (AST.IntLit n2) : RVLit (AST.IntLit n1) : vs', _) -> case bo of
        AST.Add -> runMachine stack' fg ls es (RVLit (AST.IntLit (n1 + n2)) : vs') ss
        AST.Sub -> runMachine stack' fg ls es (RVLit (AST.IntLit (n1 - n2)) : vs') ss
        AST.NotEq -> runMachine stack' fg ls es (RVLit (AST.IntLit (if n1 /= n2 then 1 else 0)) : vs') ss
    (_, _, _) -> Left "Not implemented yet"
