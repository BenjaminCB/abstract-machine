module Parser where

import AST
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

languageDef :: Token.LanguageDef ()
languageDef =
    emptyDef
        { Token.identStart = letter
        , Token.identLetter = alphaNum
        , Token.reservedNames =
            [ "let"
            , "if"
            , "else"
            , "while"
            , "comp"
            , "export"
            , "import"
            , "from"
            , "as"
            , "*"
            ]
        , Token.commentLine = "--"
        }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

integer :: Parser Integer
integer = Token.integer lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

semi :: Parser String
semi = Token.semi lexer

comma :: Parser String
comma = Token.comma lexer

parseInput :: String -> Either ParseError (SrcFile String)
parseInput = parse (whiteSpace >> srcFile) ""

srcFile :: Parser (SrcFile String)
srcFile = do
    imports <- many importDecl
    stmts <- many statement
    exports <- many exportDecl
    return $ SrcFile imports stmts exports

-- import * as x from "path"
-- import { x, y } from "path"
importDecl :: Parser (Import String)
importDecl = (try importStar <|> importList) <* semi
    where
        importStar = do
            reserved "import"
            reserved "*"
            reserved "as"
            alias <- identifier
            reserved "from"
            ImportStar alias <$> stringLiteral
        importList = do
            reserved "import"
            ids <- braces (identifier `sepBy` comma)
            reserved "from"
            ImportList ids <$> stringLiteral
        stringLiteral = Token.stringLiteral lexer

exportDecl :: Parser Export
exportDecl = do
    reserved "export"
    ident <- identifier
    _ <- semi
    return $ Export ident

statement :: Parser Stmt
statement = whiteSpace *> choice [ifStmt, whileStmt, compCallStmt, letStmt, assignStmt] <* semi
    where
        ifStmt = do
            reserved "if"
            cond <- parens expression
            thenStmts <- braces $ many statement
            reserved "else"
            elseStmts <- braces $ many statement
            return $ If cond thenStmts elseStmts
        whileStmt = do
            reserved "while"
            cond <- parens expression
            stmts <- braces $ many statement
            return $ While cond stmts
        compCallStmt = do
            reserved "comp"
            compExpr <- expression
            args <- parens $ expression `sepBy` comma
            return $ CompCall compExpr args
        letStmt = do
            reserved "let"
            var <- identifier
            reserved "="
            Let var <$> expression
        assignStmt = do
            var <- identifier
            reserved "="
            Assign var <$> expression

expression :: Parser Expr
expression = choice [try binOpExpr, try projExpr, try litExpr, varExpr]
    where
        binOpExpr = do
            op <- binOp
            spaces
            e1 <- expression
            spaces
            BO op e1 <$> expression
        binOp = choice [Add <$ string "+", Sub <$ string "-", NotEq <$ string "!="]
        projExpr = do
            obj <- identifier
            _ <- char '.'
            Proj obj <$> identifier
        litExpr = Lit <$> literal
        varExpr = Var <$> identifier

literal :: Parser Lit
literal = int <|> comp
    where
        int = IntLit . fromIntegral <$> integer
        comp = do
            _ <- string "<"
            args <- identifier `sepBy` comma
            _ <- string ">"
            stmts <- many statement
            _ <- string "</>"
            return $ CompLit args stmts
