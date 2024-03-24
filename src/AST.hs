module AST where

data SrcFile = SrcFile [Import] [Stmt] [Export]
    deriving (Eq)

instance Show SrcFile where
    show (SrcFile imports stmts exports) = unlines
        [ "Imports:"
        , show imports
        , "Statements:"
        , show stmts
        , "Exports:"
        , show exports
        ]

data Import
    = ImportStar String String
    | ImportList [String] String
    deriving (Eq)

instance Show Import where
    show (ImportStar alias modName) = "import * as " ++ alias ++ " from " ++ modName
    show (ImportList names modName) = "import {" ++ unwords names ++ "} from " ++ modName

newtype Export = Export String
    deriving (Eq)

instance Show Export where
    show (Export name) = "export " ++ name

data Stmt
    = Let String Expr
    | Assign String Expr
    | If Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    | CompCall Expr [Expr]
    deriving (Eq)

instance Show Stmt where
    show (Let name expr) = "let " ++ name ++ " = " ++ show expr
    show (Assign name expr) = name ++ " = " ++ show expr
    show (If cond thenStmts elseStmts) = concat
        [ "if (" ++ show cond ++ ") {"
        , show thenStmts
        , "} else {"
        , show elseStmts
        , "}"
        ]
    show (While cond stmts) = concat
        [ "while (" ++ show cond ++ ") {"
        , show stmts
        , "}"
        ]
    show (CompCall expr args) = "comp " ++ show expr ++ "(" ++ unwords (map show args) ++ ")"

data Expr
    = Var String
    | Lit Lit
    | Proj String String
    | BO BinOp Expr Expr
    deriving (Eq)

instance Show Expr where
    show (Var name) = name
    show (Lit lit) = show lit
    show (Proj obj field) = obj ++ "." ++ field
    show (BO op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2

data Lit
    = IntLit Int
    | CompLit [String] [Stmt]
    deriving (Eq)

instance Show Lit where
    show (IntLit n) = show n
    show (CompLit args stmts) = concat
        [ "<" ++ unwords args ++ ">"
        , show stmts
        , "</>"
        ]

data BinOp
    = Add
    | Sub
    | NotEq
    deriving (Eq)

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show NotEq = "!="
