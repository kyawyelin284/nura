module Parser
    ( parseExpr
    , parseProgram
    ) where

import AST (Expr(..), Pattern(..), Type(..), Decl(..), Constructor(..))
import Control.Applicative (many, optional, some)
import Control.Monad (void)
import Data.List (isSuffixOf)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, manyTill, notFollowedBy, parse, sepBy, try, (<|>))
import Text.Megaparsec.Char (char, digitChar, letterChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

type Parser = Parsec Void String

parseExpr :: String -> Either String Expr
parseExpr input =
    case parse (sc *> exprParser <* eof) "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right expr -> Right expr

parseProgram :: String -> Either String ([FilePath], [Decl], Expr)
parseProgram input =
    case traverse parseStmt (splitStatements input) of
        Left err -> Left err
        Right stmts ->
            let imports = [path | StmtImport path <- stmts]
                decls = [decl | StmtDecl decl <- stmts]
            in case buildExpr stmts of
                Nothing -> Left "expected expression"
                Just expr -> Right (imports, decls, expr)

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
reservedWords = ["let", "rec", "in", "if", "then", "else", "true", "false", "fun", "match", "with", "import", "type"]

keyword :: String -> Parser ()
keyword word = lexeme (string word *> notFollowedBy letterChar)

identifier :: Parser String
identifier = lexeme $ try $ do
    first <- letterChar
    rest <- many (letterChar <|> digitChar)
    let name = first : rest
    if name `elem` reservedWords
        then fail ("reserved word " <> show name)
        else pure name

constructorIdentifier :: Parser String
constructorIdentifier = lexeme $ try $ do
    first <- upperChar
    rest <- many (letterChar <|> digitChar)
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

stringLiteralExpr :: Parser Expr
stringLiteralExpr = StringLit <$> stringLiteral

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
    params <- many identifier
    void (symbol "=")
    valueExpr <- exprParser
    keyword "in"
    bodyExpr <- exprParser
    let valueExpr' = foldr Lambda valueExpr params
    if isRec
        then pure (LetRec name valueExpr' bodyExpr)
        else pure (Let name valueExpr' bodyExpr)

ifExpr :: Parser Expr
ifExpr = lexeme $ do
    keyword "if"
    condExpr <- exprParser
    keyword "then"
    thenExpr <- exprParser
    keyword "else"
    elseExpr <- exprParser
    pure (If condExpr thenExpr elseExpr)

listLiteral :: Parser Expr
listLiteral = do
    elements <- between (symbol "[") (symbol "]") (exprParser `sepBy` symbol ",")
    case elements of
        [] -> pure Nil
        _ -> pure (ListLit elements)

constructorExpr :: Parser Expr
constructorExpr = lexeme $ do
    name <- constructorIdentifier
    args <- optional (parens (exprParser `sepBy` symbol ","))
    pure (ConstructorExpr name (maybe [] id args))

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
    <|> constructorExpr
    <|> listLiteral
    <|> parens exprParser
    <|> stringLiteralExpr
    <|> booleanLiteral
    <|> integer
    <|> variable

application :: Parser Expr
application = do
    func <- atom
    argLists <- many (parens (exprParser `sepBy` symbol ","))
    let args = concat argLists
    pure (foldl Apply func args)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixL (Mul <$ symbol "*") ]
    , [ InfixL (Add <$ symbol "+")
      , InfixL (Sub <$ symbol "-")
      ]
    , [ InfixN (GreaterThanOrEqual <$ symbol ">=")
      , InfixN (GreaterThan <$ symbol ">")
      , InfixN (LessThanOrEqual <$ symbol "<=")
      , InfixN (LessThan <$ symbol "<")
      ]
    , [ InfixN (NotEqual <$ symbol "!=")
      , InfixN (Equal <$ symbol "==")
      ]
    ]

exprParser :: Parser Expr
exprParser = seqExpr

exprParserNoSeq :: Parser Expr
exprParserNoSeq = makeExprParser application operatorTable

seqExpr :: Parser Expr
seqExpr = do
    first <- makeExprParser application operatorTable
    rest <- many (seqSep *> makeExprParser application operatorTable)
    pure (foldl Seq first rest)
  where
    seqSep = do
        _ <- symbol ";"
        notFollowedBy (char ';')

data Stmt
    = StmtImport FilePath
    | StmtDecl Decl
    | StmtDef Bool String Expr
    | StmtExpr Expr

parseStmt :: String -> Either String Stmt
parseStmt input =
    case parse (sc *> stmtParser <* eof) "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right stmt -> Right stmt

stmtParser :: Parser Stmt
stmtParser = do
    stmt <-
        try (StmtImport <$> importStmt)
        <|> try (StmtDecl <$> typeDecl)
        <|> try topLetDef
        <|> StmtExpr <$> exprParser
    void (many (symbol ";"))
    pure stmt

importStmt :: Parser FilePath
importStmt = lexeme $ do
    keyword "import"
    path <- stringLiteral
    pure path

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

typeDecl :: Parser Decl
typeDecl = lexeme $ do
    keyword "type"
    typeName <- constructorIdentifier
    typeParams <- many identifier
    void (symbol "=")
    ctors <- constructorDecl `sepBy` symbol "|"
    case ctors of
        [] -> fail "expected at least one constructor"
        _ -> pure (TypeDecl typeName typeParams ctors)

constructorDecl :: Parser Constructor
constructorDecl = do
    name <- constructorIdentifier
    args <- many typeExpr
    pure (Constructor name args)

typeAtom :: Parser Type
typeAtom =
    parens typeExpr
    <|> listType
    <|> typeConstructor
    <|> typeVar

typeExpr :: Parser Type
typeExpr =
    try typeConstructorApp
    <|> typeAtom

typeConstructorApp :: Parser Type
typeConstructorApp = do
    name <- constructorIdentifier
    args <- some typeAtom
    pure (TCon name args)

typeConstructor :: Parser Type
typeConstructor = TCon <$> constructorIdentifier <*> pure []

typeVar :: Parser Type
typeVar = TVar <$> identifier

listType :: Parser Type
listType = TList <$> between (symbol "[") (symbol "]") typeExpr

topLetDef :: Parser Stmt
topLetDef = lexeme $ do
    keyword "let"
    isRec <- (True <$ keyword "rec") <|> pure False
    name <- identifier
    params <- many identifier
    void (symbol "=")
    valueExpr <- exprParserNoSeq
    notFollowedBy (keyword "in")
    let valueExpr' = foldr Lambda valueExpr params
    pure (StmtDef isRec name valueExpr')

buildExpr :: [Stmt] -> Maybe Expr
buildExpr stmts =
    go (reverse stmts) Nothing
  where
    go remaining acc =
        case remaining of
            [] -> acc
            stmt : rest ->
                case stmt of
                    StmtExpr expr ->
                        let acc' = case acc of
                                Nothing -> Just expr
                                Just next -> Just (Seq expr next)
                        in go rest acc'
                    StmtDef isRec name valueExpr ->
                        case acc of
                            Nothing -> Nothing
                            Just next ->
                                let wrapped =
                                        if isRec
                                            then LetRec name valueExpr next
                                            else Let name valueExpr next
                                in go rest (Just wrapped)
                    _ -> go rest acc

splitStatements :: String -> [String]
splitStatements input =
    let (statements, current, _) = foldl step ([], [], False) (lines input)
        finalStatements = if null current then statements else statements ++ [current]
    in map (normalizeStatement . unlines) finalStatements
  where
    step (statements, current, inBlock) line =
        let trimmed = dropWhile (\c -> c == ' ' || c == '\t') line
            isBlank = null trimmed
            startsWith prefix = take (length prefix) trimmed == prefix
            isDecl = startsWith "type" || startsWith "import"
            startsStmt = startsWith "let" || startsWith "print" || startsWith "match" || isDecl
            currentIsDecl =
                case current of
                    [] -> False
                    firstLine : _ ->
                        let firstTrimmed = dropWhile (\c -> c == ' ' || c == '\t') firstLine
                        in take 4 firstTrimmed == "type" || take 6 firstTrimmed == "import"
            prevLineContinues =
                case reverse current of
                    [] -> False
                    lastLine : _ -> lineContinuesForSplit lastLine
            inBlock' = inBlock || prevLineContinues
        in if isBlank
            then if null current
                then (statements, [], False)
                else (statements ++ [current], [], False)
            else if currentIsDecl
                then (statements ++ [current], [line], False)
            else if (startsStmt && not prevLineContinues && not (null current) && not inBlock)
                then (statements ++ [current], [line], False)
            else if isDecl && not (null current)
                then (statements ++ [current], [line], False)
                else (statements, current ++ [line], inBlock')

    lineContinuesForSplit line =
        let trimmedLine = reverse (dropWhile (\c -> c == ' ' || c == '\t') (reverse line))
        in trimmedLine `elem` ["in", "then", "else", "with"]
            || any (`isSuffixOf` trimmedLine) [" in", " then", " else", " with", "->", "="]

normalizeStatement :: String -> String
normalizeStatement statement =
    unlines (addSeps (lines statement))
  where
    addSeps ls =
        case ls of
            [] -> []
            [line] -> [line]
            line : next : rest ->
                let trimmedNext = dropWhile (\c -> c == ' ' || c == '\t') next
                    startsWith prefix = take (length prefix) trimmedNext == prefix
                    nextIsContinuation =
                        startsWith "|" || startsWith "in" || startsWith "then" || startsWith "else"
                    trimmedLine = reverse (dropWhile (\c -> c == ' ' || c == '\t') (reverse line))
                    lineContinues =
                        trimmedLine `elem` ["in", "then", "else", "with"]
                        || any (`isSuffixOf` trimmedLine) [" in", " then", " else", " with", "->", "="]
                    line' =
                        if not nextIsContinuation && not lineContinues
                            then line <> ";"
                            else line
                in line' : addSeps (next : rest)

