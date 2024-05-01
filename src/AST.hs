module AST where

data SrcFile a = SrcFile [Import a] [Stmt] [Export]
    deriving (Show, Eq)

instance Functor SrcFile where
    fmap f (SrcFile is ss es) = SrcFile (map (fmap f) is) ss es

data Import a
    = ImportStar String a
    | ImportList [String] a
    deriving (Show, Eq)

instance Functor Import where
    fmap f (ImportStar s a) = ImportStar s (f a)
    fmap f (ImportList ss a) = ImportList ss (f a)

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
