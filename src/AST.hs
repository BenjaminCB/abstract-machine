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
    | For String Int Int [Stmt]
    deriving (Show, Eq)

data Expr
    = Var String
    | Lit Lit
    | Proj String String
    | BO BinOp Expr Expr
    deriving (Eq)

instance Show Expr where
    show (Var x) = x
    show (Lit l) = show l
    show (Proj x y) = x ++ "." ++ y
    show (BO op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2

data Lit
    = IntLit Int
    | CompLit [String] [Stmt]
    deriving (Eq)

instance Show Lit where
    show (IntLit i) = show i
    show (CompLit ss stmts) = "comp(" ++ show ss ++ ", " ++ show stmts ++ ")"

data BinOp
    = Add
    | Sub
    | NotEq
    deriving (Eq)

instance Show BinOp where
    show Add = "+"
    show Sub = "-"
    show NotEq = "!="
