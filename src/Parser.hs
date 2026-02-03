module Parser
    ( parseExpr
    , parseProgram
    ) where

import AST (Expr(..), Pattern(..))
import Control.Applicative (many, optional, some)
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, manyTill, notFollowedBy, parse, sepBy, try, (<|>))
import Text.Megaparsec.Char (char, letterChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

type Parser = Parsec Void String

parseExpr :: String -> Either String Expr
parseExpr input =
    case parse (sc *> exprParser <* eof) "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right expr -> Right expr

parseProgram :: String -> Either String ([FilePath], Expr)
parseProgram input =
    case parse (sc *> programParser <* eof) "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right result -> Right result

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

reservedWords :: [String]
reservedWords = ["let", "rec", "in", "print", "if", "then", "else", "true", "false", "fun", "match", "with", "import"]

keyword :: String -> Parser ()
keyword word = lexeme (string word *> notFollowedBy letterChar)

identifier :: Parser String
identifier = lexeme $ try $ do
    name <- some letterChar
    if name `elem` reservedWords
        then fail ("reserved word " <> show name)
        else pure name

constructorIdentifier :: Parser String
constructorIdentifier = lexeme $ try $ do
    first <- upperChar
    rest <- many letterChar
    let name = first : rest
    if name `elem` reservedWords
        then fail ("reserved word " <> show name)
        else pure name

integer :: Parser Expr
integer = IntLit <$> lexeme L.decimal

booleanLiteral :: Parser Expr
booleanLiteral =
    (BoolLit True <$ keyword "true")
    <|> (BoolLit False <$ keyword "false")

variable :: Parser Expr
variable = Var <$> identifier

lambdaExpr :: Parser Expr
lambdaExpr = lexeme $ do
    keyword "fun"
    name <- identifier
    void (symbol "->")
    bodyExpr <- exprParser
    pure (Lambda name bodyExpr)

letBinding :: Parser Expr
letBinding = lexeme $ do
    keyword "let"
    isRec <- (True <$ keyword "rec") <|> pure False
    name <- identifier
    void (symbol "=")
    valueExpr <- exprParser
    keyword "in"
    bodyExpr <- exprParser
    if isRec
        then pure (LetRec name valueExpr bodyExpr)
        else pure (Let name valueExpr bodyExpr)

ifExpr :: Parser Expr
ifExpr = lexeme $ do
    keyword "if"
    condExpr <- exprParser
    keyword "then"
    thenExpr <- exprParser
    keyword "else"
    elseExpr <- exprParser
    pure (If condExpr thenExpr elseExpr)

printCall :: Parser Expr
printCall = lexeme $ do
    keyword "print"
    expr <- parens exprParser
    pure (Print expr)

listLiteral :: Parser Expr
listLiteral = do
    elements <- between (symbol "[") (symbol "]") (exprParser `sepBy` symbol ",")
    case elements of
        [] -> pure Nil
        _ -> pure (ListLit elements)

matchExpr :: Parser Expr
matchExpr = lexeme $ do
    keyword "match"
    scrutinee <- exprParser
    keyword "with"
    void (symbol "|")
    pattern1 <- patternParser
    void (symbol "->")
    branch1 <- exprParser
    void (symbol "|")
    pattern2 <- patternParser
    void (symbol "->")
    branch2 <- exprParser
    pure (Match scrutinee pattern1 branch1 pattern2 branch2)

patternParser :: Parser Pattern
patternParser =
    listNilPattern
    <|> consPattern
    <|> ctorPattern

listNilPattern :: Parser Pattern
listNilPattern = PNil <$ (symbol "[" *> symbol "]")

consPattern :: Parser Pattern
consPattern = try $ do
    headName <- identifier
    void (symbol ":")
    tailName <- identifier
    pure (PCons headName tailName)

ctorPattern :: Parser Pattern
ctorPattern = do
    name <- constructorIdentifier
    args <- optional (parens (identifier `sepBy` symbol ","))
    pure (PCtor name (maybe [] id args))

atom :: Parser Expr
atom =
    letBinding
    <|> ifExpr
    <|> lambdaExpr
    <|> matchExpr
    <|> printCall
    <|> listLiteral
    <|> parens exprParser
    <|> booleanLiteral
    <|> integer
    <|> variable

application :: Parser Expr
application = do
    func <- atom
    args <- many (parens exprParser)
    pure (foldl Apply func args)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixL (Mul <$ symbol "*") ]
    , [ InfixL (Add <$ symbol "+")
      , InfixL (Sub <$ symbol "-")
      ]
    , [ InfixN (GreaterThan <$ symbol ">") ]
    ]

exprParser :: Parser Expr
exprParser = makeExprParser application operatorTable

programParser :: Parser ([FilePath], Expr)
programParser = do
    imports <- many importStmt
    expr <- exprParser
    pure (imports, expr)

importStmt :: Parser FilePath
importStmt = lexeme $ do
    keyword "import"
    path <- stringLiteral
    pure path

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')
