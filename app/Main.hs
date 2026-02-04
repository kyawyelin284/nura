module Main (main) where

import AST (Expr(..), Type(..), Decl(..), Constructor(..))
import Bytecode (compile)
import Interpreter (builtinEnv, eval)
import Parser (parseProgram)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hFlush, isEOF, stdout)
import qualified Data.Set as Set
import TypeChecker (CtorEnv, TypeEnv, infer)
import VM (runVM)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--version"] -> putStrLn "Nura v1.0"
        [] -> runRepl
        _ ->
            case parseArgs args of
                Right (UseInterpreter, path) -> runInterpreter path
                Right (UseVM, path) -> runVMMode path
                Left usage -> putStrLn usage

data Mode
    = UseInterpreter
    | UseVM
    deriving (Show, Eq)

parseArgs :: [String] -> Either String (Mode, FilePath)
parseArgs args =
    let (vmFlags, paths) = partitionArgs args
        mode = if vmFlags then UseVM else UseInterpreter
    in case paths of
        [path] -> Right (mode, path)
        _ -> Left "Usage: nura [--vm] <file>"

partitionArgs :: [String] -> (Bool, [String])
partitionArgs = foldr step (False, [])
  where
    step arg (hasVm, files)
        | arg == "--vm" = (True, files)
        | otherwise = (hasVm, arg : files)

runInterpreter :: FilePath -> IO ()
runInterpreter path = do
    logDebug "Main.runInterpreter" "Start" [("path", path)]
    result <- loadWithImports path
    case result of
        Left err -> do
            logDebug "Main.runInterpreter" "Load error" [("error", err)]
            putStrLn err
        Right (expr, ctorEnv) ->
            case infer ctorEnv builtinsTypeEnv expr of
                Left err -> do
                    logDebug "Main.runInterpreter" "Type error" [("error", err)]
                    putStrLn ("Type error:\n" <> err)
                Right _ -> do
                    _ <- eval builtinEnv expr
                    pure ()

runVMMode :: FilePath -> IO ()
runVMMode path = do
    logDebug "Main.runVMMode" "Start" [("path", path)]
    result <- loadWithImports path
    case result of
        Left err -> do
            logDebug "Main.runVMMode" "Load error" [("error", err)]
            putStrLn err
        Right (expr, ctorEnv) ->
            case infer ctorEnv builtinsTypeEnv expr of
                Left err -> do
                    logDebug "Main.runVMMode" "Type error" [("error", err)]
                    putStrLn ("Type error:\n" <> err)
                Right _ ->
                    case compile expr of
                        Left feature -> do
                            logDebug "Main.runVMMode" "Compile unsupported" [("feature", feature)]
                            putStrLn ("VM backend does not support " <> feature <> " yet.")
                        Right bytecode ->
                            runVM bytecode

builtinsTypeEnv :: TypeEnv
builtinsTypeEnv =
    [ ("print", TFunc (TVar "a_print") TUnit)
    , ("println", TFunc (TVar "a_println") TUnit)
    , ("head", TFunc (TList (TVar "a_head")) (TVar "a_head"))
    , ("tail", TFunc (TList (TVar "a_tail")) (TList (TVar "a_tail")))
    , ("isEmpty", TFunc (TList (TVar "a_isEmpty")) TBool)
    , ("length", TFunc (TList (TVar "a_length")) TInt)
    , ("strlen", TFunc TString TInt)
    ]

builtinsCtorEnv :: CtorEnv
builtinsCtorEnv =
    [ ("Nil", (["a"], [], TList (TVar "a")))
    , ("Cons", (["a"], [TVar "a", TList (TVar "a")], TList (TVar "a")))
    ]

declsToCtorEnv :: [Decl] -> CtorEnv
declsToCtorEnv decls =
    concatMap declToCtors decls
  where
    declToCtors (TypeDecl typeName params ctors) =
        let resultType = TCon typeName (map TVar params)
        in map (\(Constructor ctorName ctorArgs) -> (ctorName, (params, ctorArgs, resultType))) ctors

data ReplState = ReplState
    { replDefs :: [(Bool, String, Expr)]
    , replDecls :: [Decl]
    , replVisited :: Set.Set FilePath
    }

runRepl :: IO ()
runRepl = do
    baseDir <- getCurrentDirectory
    replLoop baseDir (ReplState [] [] Set.empty)

replLoop :: FilePath -> ReplState -> IO ()
replLoop baseDir state = do
    putStr "nura> "
    hFlush stdout
    end <- isEOF
    if end
        then putStrLn ""
        else do
            line <- getLine
            case line of
                ":quit" -> pure ()
                ":q" -> pure ()
                "" -> replLoop baseDir state
                _ -> do
                    result <- runReplLine baseDir state line
                    case result of
                        Left err -> do
                            putStrLn err
                            replLoop baseDir state
                        Right newState -> replLoop baseDir newState

runReplLine :: FilePath -> ReplState -> String -> IO (Either String ReplState)
runReplLine baseDir state input =
    case parseProgram input of
        Left err -> pure (Left ("Parse error:\n" <> err))
        Right (imports, localDecls, expr) -> do
            importsResult <- loadImports baseDir imports (replVisited state)
            case importsResult of
                Left err -> pure (Left err)
                Right (importDefs, importDecls, visited') -> do
                    let (localDefs, bodyExpr) = collectDefs expr
                    let allDefs = replDefs state ++ importDefs ++ localDefs
                    let allDecls = replDecls state ++ importDecls ++ localDecls
                    let wrappedExpr = wrapDefs allDefs bodyExpr
                    let printedExpr = Apply (Var "println") wrappedExpr
                    let ctorEnv = builtinsCtorEnv ++ declsToCtorEnv allDecls
                    case infer ctorEnv builtinsTypeEnv printedExpr of
                        Left err -> pure (Left ("Type error:\n" <> err))
                        Right _ ->
                            case compile printedExpr of
                                Left feature ->
                                    pure (Left ("VM backend does not support " <> feature <> " yet."))
                                Right bytecode -> do
                                    runVM bytecode
                                    pure (Right (ReplState allDefs allDecls visited'))

loadWithImports :: FilePath -> IO (Either String (Expr, CtorEnv))
loadWithImports path = do
    absPath <- makeAbsolute path
    logDebug "Main.loadWithImports" "Read file" [("path", absPath)]
    contents <- readFile absPath
    case parseProgram contents of
        Left err -> do
            logDebug "Main.loadWithImports" "Parse error" [("path", absPath), ("error", err)]
            pure (Left ("Parse error in " <> absPath <> ":\n" <> err))
        Right (imports, localDecls, expr) -> do
            logDebug "Main.loadWithImports" "Parsed program" [("imports", show (length imports))]
            let baseDir = takeDirectory absPath
            result <- loadImports baseDir imports (Set.singleton absPath)
            case result of
                Left err -> pure (Left err)
                Right (defs, importDecls, _) -> do
                    let allDecls = localDecls ++ importDecls
                    let ctorEnv = builtinsCtorEnv ++ declsToCtorEnv allDecls
                    pure (Right (wrapDefs defs expr, ctorEnv))

loadImports :: FilePath -> [FilePath] -> Set.Set FilePath -> IO (Either String ([(Bool, String, Expr)], [Decl], Set.Set FilePath))
loadImports baseDir paths visited =
    foldl loadStep (pure (Right ([], [], visited))) paths
  where
    loadStep ioAcc importPath = do
        acc <- ioAcc
        case acc of
            Left err -> pure (Left err)
            Right (defs, decls, seen) -> do
                let resolved =
                        if isAbsolute importPath
                            then importPath
                            else baseDir </> importPath
                absPath <- makeAbsolute resolved
                logDebug "Main.loadImports" "Import" [("path", absPath)]
                if Set.member absPath seen
                    then pure (Right (defs, decls, seen))
                    else do
                        contents <- readFile absPath
                        case parseProgram contents of
                            Left err ->
                                pure (Left ("Parse error in " <> absPath <> ":\n" <> err))
                            Right (imports, localDecls, expr) -> do
                                let baseDir' = takeDirectory absPath
                                nested <- loadImports baseDir' imports (Set.insert absPath seen)
                                case nested of
                                    Left err -> pure (Left err)
                                    Right (nestedDefs, nestedDecls, seen') ->
                                        let (localDefs, _) = collectDefs expr
                                        in pure (Right (defs ++ nestedDefs ++ localDefs, decls ++ nestedDecls ++ localDecls, seen'))

collectDefs :: Expr -> ([(Bool, String, Expr)], Expr)
collectDefs expr =
    case expr of
        Let name valueExpr bodyExpr ->
            let (defs, rest) = collectDefs bodyExpr
            in ((False, name, valueExpr) : defs, rest)
        LetRec name valueExpr bodyExpr ->
            let (defs, rest) = collectDefs bodyExpr
            in ((True, name, valueExpr) : defs, rest)
        _ -> ([], expr)

wrapDefs :: [(Bool, String, Expr)] -> Expr -> Expr
wrapDefs defs expr =
    foldr wrap expr defs
  where
    wrap (isRec, name, valueExpr) acc =
        if isRec
            then LetRec name valueExpr acc
            else Let name valueExpr acc

logDebug :: String -> String -> [(String, String)] -> IO ()
logDebug _ _ _ = pure ()
