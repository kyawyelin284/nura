module Interpreter
    ( Value(..)
    , Builtin(..)
    , Env
    , builtinEnv
    , eval
    ) where

import AST (Expr(..), Pattern(..))
import Control.Monad.Fix (mfix)

data Builtin
    = BuiltinPrint
    | BuiltinPrintln
    | BuiltinHead
    | BuiltinTail
    | BuiltinIsEmpty
    | BuiltinLength
    | BuiltinStrLen
    deriving (Show, Eq)

data Value
    = IntVal Integer
    | BoolVal Bool
    | StringVal String
    | Closure String Expr Env
    | ListVal [Value]
    | ConstructorVal String [Value]
    | Builtin Builtin
    | Unit
    deriving (Show, Eq)

type Env = [(String, Value)]

builtinEnv :: Env
builtinEnv =
    [ ("print", Builtin BuiltinPrint)
    , ("println", Builtin BuiltinPrintln)
    , ("head", Builtin BuiltinHead)
    , ("tail", Builtin BuiltinTail)
    , ("isEmpty", Builtin BuiltinIsEmpty)
    , ("length", Builtin BuiltinLength)
    , ("strlen", Builtin BuiltinStrLen)
    ]

eval :: Env -> Expr -> IO Value
eval _ (IntLit n) = pure (IntVal n)
eval _ (BoolLit b) = pure (BoolVal b)
eval _ (StringLit s) = pure (StringVal s)
eval env (Var name) =
    case lookup name env of
        Just value -> pure value
        Nothing -> fail ("unbound variable: " <> name)
eval env (Lambda name bodyExpr) = pure (Closure name bodyExpr env)
eval env (Apply funcExpr argExpr) = do
    funcValue <- eval env funcExpr
    case funcValue of
        Closure name bodyExpr closureEnv -> do
            argValue <- eval env argExpr
            eval ((name, argValue) : closureEnv) bodyExpr
        Builtin builtin -> do
            argValue <- eval env argExpr
            applyBuiltin builtin argValue
        _ -> fail "attempted to apply non-function value"
eval env (ListLit elements) = do
    values <- mapM (eval env) elements
    pure (foldr (\h t -> ConstructorVal "Cons" [h, t]) (ConstructorVal "Nil" []) values)
eval _ Nil = pure (ConstructorVal "Nil" [])
eval env (Cons headExpr tailExpr) = do
    headValue <- eval env headExpr
    tailValue <- eval env tailExpr
    case tailValue of
        ListVal values -> pure (ListVal (headValue : values))
        ConstructorVal "Nil" [] -> pure (ConstructorVal "Cons" [headValue, ConstructorVal "Nil" []])
        ConstructorVal "Cons" _ -> pure (ConstructorVal "Cons" [headValue, tailValue])
        _ -> fail "attempted to cons onto non-list value"
eval env (ConstructorExpr name args) = do
    values <- mapM (eval env) args
    pure (ConstructorVal name values)
eval env (Add left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case (leftVal, rightVal) of
        (IntVal l, IntVal r) -> pure (IntVal (l + r))
        (StringVal l, StringVal r) -> pure (StringVal (l <> r))
        _ -> fail "attempted to add non-numeric or non-string values"
eval env (Sub left right) = do
    IntVal l <- eval env left
    IntVal r <- eval env right
    pure (IntVal (l - r))
eval env (Mul left right) = do
    IntVal l <- eval env left
    IntVal r <- eval env right
    pure (IntVal (l * r))
eval env (GreaterThan left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case compareOrderValues leftVal rightVal of
        Just GT -> pure (BoolVal True)
        Just _ -> pure (BoolVal False)
        Nothing -> fail "attempted to compare non-matching values"
eval env (LessThan left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case compareOrderValues leftVal rightVal of
        Just LT -> pure (BoolVal True)
        Just _ -> pure (BoolVal False)
        Nothing -> fail "attempted to compare non-matching values"
eval env (LessThanOrEqual left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case compareOrderValues leftVal rightVal of
        Just GT -> pure (BoolVal False)
        Just _ -> pure (BoolVal True)
        Nothing -> fail "attempted to compare non-matching values"
eval env (GreaterThanOrEqual left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case compareOrderValues leftVal rightVal of
        Just LT -> pure (BoolVal False)
        Just _ -> pure (BoolVal True)
        Nothing -> fail "attempted to compare non-matching values"
eval env (Equal left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case compareValues leftVal rightVal of
        Just result -> pure (BoolVal result)
        Nothing -> fail "attempted to compare non-matching values"
eval env (NotEqual left right) = do
    leftVal <- eval env left
    rightVal <- eval env right
    case compareValues leftVal rightVal of
        Just result -> pure (BoolVal (not result))
        Nothing -> fail "attempted to compare non-matching values"
eval env (If condExpr thenExpr elseExpr) = do
    BoolVal cond <- eval env condExpr
    if cond
        then eval env thenExpr
        else eval env elseExpr
eval env (Match scrutinee pattern1 branch1 pattern2 branch2) = do
    scrutineeValue <- eval env scrutinee
    case matchPattern pattern1 scrutineeValue of
        Just bindings ->
            eval (bindings ++ env) branch1
        Nothing ->
            case matchPattern pattern2 scrutineeValue of
                Just bindings -> eval (bindings ++ env) branch2
                Nothing -> fail "non-exhaustive match"
eval env (Let name valueExpr bodyExpr) = do
    value <- eval env valueExpr
    eval ((name, value) : env) bodyExpr
eval env (LetRec name valueExpr bodyExpr) = do
    value <- mfix (\v -> eval ((name, v) : env) valueExpr)
    eval ((name, value) : env) bodyExpr
eval env (Seq firstExpr secondExpr) = do
    _ <- eval env firstExpr
    eval env secondExpr
eval env (Print expr) = do
    value <- eval env expr
    putStrLn (renderValue value)
    pure Unit

applyBuiltin :: Builtin -> Value -> IO Value
applyBuiltin builtin value =
    case builtin of
        BuiltinPrint -> do
            putStrLn (renderValue value)
            pure Unit
        BuiltinPrintln -> do
            putStrLn (renderValue value)
            pure Unit
        BuiltinHead ->
            case value of
                ListVal (headValue : _) -> pure headValue
                ConstructorVal "Cons" (headValue : _) -> pure headValue
                ConstructorVal "Nil" [] -> fail "head: empty list"
                _ -> fail "head: expected list"
        BuiltinTail ->
            case value of
                ListVal (_ : tailValues) -> pure (ListVal tailValues)
                ConstructorVal "Cons" [_, tailValue] -> pure tailValue
                ConstructorVal "Nil" [] -> fail "tail: empty list"
                _ -> fail "tail: expected list"
        BuiltinIsEmpty ->
            case value of
                ListVal [] -> pure (BoolVal True)
                ListVal _ -> pure (BoolVal False)
                ConstructorVal "Nil" [] -> pure (BoolVal True)
                ConstructorVal "Cons" _ -> pure (BoolVal False)
                _ -> fail "isEmpty: expected list"
        BuiltinLength ->
            case value of
                ListVal values -> pure (IntVal (fromIntegral (length values)))
                ConstructorVal "Nil" [] -> pure (IntVal 0)
                ConstructorVal "Cons" _ -> pure (IntVal (lengthConstructorList 0 value))
                _ -> fail "length: expected list"
        BuiltinStrLen ->
            case value of
                StringVal s -> pure (IntVal (fromIntegral (length s)))
                _ -> fail "strlen: expected string"

lengthConstructorList :: Integer -> Value -> Integer
lengthConstructorList acc value =
    case value of
        ConstructorVal "Nil" [] -> acc
        ConstructorVal "Cons" [_, tailValue] -> lengthConstructorList (acc + 1) tailValue
        _ -> acc

renderValue :: Value -> String
renderValue value =
    case value of
        IntVal n -> show n
        BoolVal b -> if b then "true" else "false"
        StringVal s -> s
        ListVal values -> "[" <> renderList values <> "]"
        ConstructorVal "Nil" [] -> "[]"
        ConstructorVal "Cons" _ -> "[" <> renderConstructorList value <> "]"
        ConstructorVal name fields ->
            name <> "(" <> renderList fields <> ")"
        Builtin _ -> "<builtin>"
        Closure _ _ _ -> "<closure>"
        Unit -> "Unit"

renderList :: [Value] -> String
renderList values =
    case values of
        [] -> ""
        [v] -> renderValue v
        v : rest -> renderValue v <> ", " <> renderList rest

renderConstructorList :: Value -> String
renderConstructorList value =
    case value of
        ConstructorVal "Nil" [] -> ""
        ConstructorVal "Cons" [headValue, tailValue] ->
            case tailValue of
                ConstructorVal "Nil" [] -> renderValue headValue
                ConstructorVal "Cons" _ -> renderValue headValue <> ", " <> renderConstructorList tailValue
                _ -> renderValue headValue <> ", " <> renderValue tailValue
        _ -> renderValue value

matchPattern :: Pattern -> Value -> Maybe Env
matchPattern pattern value =
    case (pattern, value) of
        (PNil, ListVal []) -> Just []
        (PNil, ConstructorVal "Nil" []) -> Just []
        (PCons headName tailName, ListVal (headValue : tailValues)) ->
            Just [(headName, headValue), (tailName, ListVal tailValues)]
        (PCons headName tailName, ConstructorVal "Cons" [headValue, tailValue]) ->
            Just [(headName, headValue), (tailName, tailValue)]
        (PCtor name vars, ConstructorVal ctorName fields)
            | name == ctorName && length vars == length fields ->
                Just (zip vars fields)
        _ -> Nothing

compareValues :: Value -> Value -> Maybe Bool
compareValues leftVal rightVal =
    case (leftVal, rightVal) of
        (IntVal l, IntVal r) -> Just (l == r)
        (BoolVal l, BoolVal r) -> Just (l == r)
        (StringVal l, StringVal r) -> Just (l == r)
        _ -> do
            leftList <- toList leftVal
            rightList <- toList rightVal
            listEquals leftList rightList

toList :: Value -> Maybe [Value]
toList value =
    case value of
        ListVal values -> Just values
        ConstructorVal "Nil" [] -> Just []
        ConstructorVal "Cons" [headValue, tailValue] -> do
            rest <- toList tailValue
            Just (headValue : rest)
        _ -> Nothing

listEquals :: [Value] -> [Value] -> Maybe Bool
listEquals left right =
    case (left, right) of
        ([], []) -> Just True
        (l : ls, r : rs) -> do
            elemEqual <- compareValues l r
            restEqual <- listEquals ls rs
            Just (elemEqual && restEqual)
        _ -> Just False

compareOrderValues :: Value -> Value -> Maybe Ordering
compareOrderValues leftVal rightVal =
    case (leftVal, rightVal) of
        (IntVal l, IntVal r) -> Just (compare l r)
        (StringVal l, StringVal r) -> Just (compare l r)
        _ ->
            case (toList leftVal, toList rightVal) of
                (Just leftList, Just rightList) ->
                    listCompare leftList rightList
                _ ->
                    case (leftVal, rightVal) of
                        (ConstructorVal leftTag leftFields, ConstructorVal rightTag rightFields) ->
                            case compare leftTag rightTag of
                                EQ -> listCompare leftFields rightFields
                                other -> Just other
                        _ -> Nothing

listCompare :: [Value] -> [Value] -> Maybe Ordering
listCompare left right =
    case (left, right) of
        ([], []) -> Just EQ
        ([], _) -> Just LT
        (_, []) -> Just GT
        (l : ls, r : rs) -> do
            elemOrder <- compareOrderValues l r
            case elemOrder of
                EQ -> listCompare ls rs
                _ -> Just elemOrder
