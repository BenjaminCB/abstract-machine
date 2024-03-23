module AST where

import Data.Map qualified as M

data SrcFile = SrcFile [Import] [Stmt] [Export]
    deriving (Eq, Show)

data Import
    = ImportStar String String
    | ImportList [String] String
    deriving (Eq, Show)

newtype Export = Export String
    deriving (Eq, Show)

data Stmt
    = Let String Expr
    | Assign String Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | CompCall Expr [Expr]
    deriving (Eq, Show)

data Expr
    = Var String
    | Lit Lit
    | Proj String String
    | BO BinOp Expr Expr
    deriving (Eq, Show)

data Lit
    = IntLit Int
    | CompLit [String] [Stmt]
    | Obj (M.Map String Lit)
    deriving (Eq, Show)

data BinOp
    = Add
    | Sub
    | NotEq
    deriving (Eq, Show)