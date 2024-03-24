module AST where

data SrcFile = SrcFile [Import] [Stmt] [Export]
    deriving (Show, Eq)

data Import
    = ImportStar String String
    | ImportList [String] String
    deriving (Show, Eq)

newtype Export = Export String
    deriving (Show, Eq)

data Stmt
    = Let String Expr
    | Assign String Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | CompCall Expr [Expr]
    deriving (Show, Eq)

data Expr
    = Var String
    | Lit Lit
    | Proj String String
    | BO BinOp Expr Expr
    deriving (Show, Eq)

data Lit
    = IntLit Int
    | CompLit [String] [Stmt]
    deriving (Show, Eq)

data BinOp
    = Add
    | Sub
    | NotEq
    deriving (Show, Eq)
