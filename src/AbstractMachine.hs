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

runFile ::
    String ->
    AST.SrcFile ->
    M.Map String AST.SrcFile ->
    Either String (M.Map (String, String) RuntimeValue)
runFile entryPoint src fileGetter =
    evalStateT (run [SrcFile src] [] [entryPoint]) (fileGetter, M.empty, M.empty, "")

updateTrace :: String -> StateT (a, b, c, String) (Either d) ()
updateTrace s = do
    (files, locals, exports, trace) <- get
    put (files, locals, exports, trace ++ s)

run ::
    [RuntimeSyntax] ->
    [RuntimeValue] ->
    [String] ->
    StateT
        ( M.Map String AST.SrcFile
        , M.Map (String, String) RuntimeValue
        , M.Map String RuntimeValue
        , String
        )
        (Either String)
        (M.Map (String, String) RuntimeValue)
-- end configuration
run [] _ _ = do
    (_, locals, _, _) <- get
    return locals

-- srcfile
run (SrcFile (AST.SrcFile imports stmts exports) : stack) vs ss =
    run (map Import imports ++ map Stmt stmts ++ map Export exports ++ stack) vs ss
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
    (_, locals, _, _) <- get
    val <- lift $ localLookup s v locals
    run stack (val : vs) (s : ss)

-- proj
run (Expr (AST.Proj ident field) : stack) vs (s : ss) = do
    (_, locals, _, _) <- get
    val <- lift $ localLookup s ident locals
    case val of
        RVObj obj -> case M.lookup field obj of
            Just lit -> run stack (lit : vs) (s : ss)
            Nothing -> lift $ Left $ "Field " ++ field ++ " not found in object " ++ ident ++ " in scope " ++ s
        _ -> lift $ Left $ "Variable " ++ ident ++ " is not an object"

-- if rules
run (Stmt (AST.If cond t f) : stack) vs ss = run (Expr cond : Branch t f : stack) vs ss
run (Branch t f : stack) (RVInt n : vs) ss =
    run (if n /= 0 then map Stmt t else map Stmt f ++ stack) vs ss
-- while rules
run (Stmt (AST.While cond body) : stack) vs ss = run (Expr cond : While' cond body : stack) vs ss
run wstack@(While' cond body : stack) (RVInt n : vs) ss =
    run (if n /= 0 then map Stmt body ++ [Expr cond] ++ wstack else stack) vs ss
-- let and assign
run (Stmt (AST.Let ident e) : stack) vs ss = run (Expr e : Bind ident : stack) vs ss
run (Bind ident : stack) (v : vs) (s : ss) = do
    (files, locals, exports, trace) <- get
    put (files, localInsert s ident v locals, exports, trace)
    run stack vs (s : ss)
run (Stmt (AST.Assign ident e) : stack) vs ss = run (Expr e : Bind ident : stack) vs ss
-- imports
run (Import (AST.ImportStar ident f) : stack) vs ss = do
    (files, _, _, _) <- get
    src <- lift $ fileLookup f files
    run (SrcFile src : PopScope : BindExports ident : EmptyExports : stack) vs (f : ss)
run (Import (AST.ImportList idents f) : stack) vs ss = do
    (files, _, _, _) <- get
    src <- lift $ fileLookup f files
    run (SrcFile src : PopScope : map BindExport idents ++ EmptyExports : stack) vs (f : ss)
run (PopScope : stack) vs (_ : ss) = run stack vs ss
run (EmptyExports : stack) vs ss = do
    (files, locals, _, trace) <- get
    put (files, locals, M.empty, trace)
    run stack vs ss
run (BindExports ident : stack) vs (s : ss) = do
    (files, locals, exports, trace) <- get
    put (files, localInsert s ident (RVObj exports) locals, exports, trace)
    run stack vs (s : ss)
run (BindExport ident : stack) vs (s : ss) = do
    (files, locals, exports, trace) <- get
    val <- lift $ exportLookup ident exports
    put (files, localInsert s ident val locals, exports, trace)
    run stack vs (s : ss)

-- export
run (Export (AST.Export ident) : stack) vs (s : ss) = do
    (files, locals, exports, trace) <- get
    val <- lift $ localLookup s ident locals
    put (files, locals, M.insert ident val exports, trace)
    run stack vs (s : ss)

-- literals
run (Expr (AST.Lit lit) : stack) vs ss = run (Lit lit : stack) vs ss
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
    (files, locals, exports, _) <- get
    let stringFiles = show $ map fst $ M.toList files
    let stringLocals = show $ M.toList $ M.map showValueConstructor locals
    let stringExports = show $ M.toList $ M.map showValueConstructor exports
    lift $
        Left $
            unlines
                [ "Failed to pattern match the following configuration:"
                , "AMStack: "
                , show $ map showSyntaxConstructor stack
                , ""
                , "AMValues: "
                , show $ map showValueConstructor vs
                , ""
                , "AMScopes: "
                , show ss
                , ""
                , "AMFiles: "
                , stringFiles
                , ""
                , "AMLocals: "
                , stringLocals
                , ""
                , "AMExports: "
                , stringExports
                ]
