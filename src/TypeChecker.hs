module TypeChecker
    ( Type(..)
    , Subst
    , applySubst
    , composeSubst
    , unify
    , CtorEnv
    , TypeEnv
    , infer
    ) where

import AST (Expr(..), Pattern(..), Type(..))
import Control.Monad (foldM)

type Subst = [(String, Type)]

applySubst :: Subst -> Type -> Type
applySubst subst ty =
    case ty of
        TInt -> TInt
        TBool -> TBool
        TUnit -> TUnit
        TVar name ->
            case lookup name subst of
                Just replacement -> replacement
                Nothing -> TVar name
        TFunc arg result ->
            TFunc (applySubst subst arg) (applySubst subst result)
        TList element ->
            TList (applySubst subst element)
        TCon name args ->
            TCon name (map (applySubst subst) args)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 =
    applyToRange s1 s2 ++ s1
  where
    applyToRange subst = map (\(name, ty) -> (name, applySubst subst ty))

unify :: Type -> Type -> Either String Subst
unify left right =
    case (left, right) of
        (TInt, TInt) -> Right []
        (TBool, TBool) -> Right []
        (TUnit, TUnit) -> Right []
        (TVar name, ty) -> bindVar name ty
        (ty, TVar name) -> bindVar name ty
        (TFunc a1 r1, TFunc a2 r2) -> do
            s1 <- unify a1 a2
            s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
            Right (composeSubst s2 s1)
        (TList l1, TList l2) -> unify l1 l2
        (TCon n1 a1, TCon n2 a2)
            | n1 == n2 && length a1 == length a2 ->
                unifyTypeLists a1 a2
        _ -> Left (typeMismatch left right)

bindVar :: String -> Type -> Either String Subst
bindVar name ty
    | ty == TVar name = Right []
    | occursIn name ty = Left ("occurs check failed: " <> name <> " in " <> show ty)
    | otherwise = Right [(name, ty)]

occursIn :: String -> Type -> Bool
occursIn name ty =
    case ty of
        TInt -> False
        TBool -> False
        TUnit -> False
        TVar other -> name == other
        TFunc arg result -> occursIn name arg || occursIn name result
        TList element -> occursIn name element
        TCon _ args -> any (occursIn name) args

type TypeEnv = [(String, Type)]

type CtorEnv = [(String, ([String], [Type], Type))]

infer :: CtorEnv -> TypeEnv -> Expr -> Either String (Subst, Type)
infer ctorEnv env expr = do
    (subst, ty, _) <- inferWith 0 ctorEnv env expr
    Right (subst, ty)

inferWith :: Int -> CtorEnv -> TypeEnv -> Expr -> Either String (Subst, Type, Int)
inferWith counter ctorEnv env expression =
    case expression of
        IntLit _ -> Right ([], TInt, counter)
        BoolLit _ -> Right ([], TBool, counter)
        Var name ->
            case lookup name env of
                Just ty ->
                    if name `elem` builtinNames
                        then
                            let (instType, nextCounter) = instantiate counter ty
                            in Right ([], instType, nextCounter)
                        else Right ([], ty, counter)
                Nothing -> Left ("unbound variable: " <> name)
        Lambda name bodyExpr -> do
            let (paramType, nextCounter) = freshTypeVar counter
            (s1, bodyType, finalCounter) <- inferWith nextCounter ctorEnv ((name, paramType) : env) bodyExpr
            Right (s1, TFunc (applySubst s1 paramType) bodyType, finalCounter)
        Apply funcExpr argExpr -> do
            (s1, funcType, c1) <- inferWith counter ctorEnv env funcExpr
            let env1 = applySubstEnv s1 env
            (s2, argType, c2) <- inferWith c1 ctorEnv env1 argExpr
            let (resultType, c3) = freshTypeVar c2
            let funcType' = applySubst s2 funcType
            case funcType' of
                TFunc paramType returnType -> do
                    s3 <- unify argType paramType
                        `orElse` funcArgMismatch funcExpr paramType argType
                    let subst = composeSubst s3 (composeSubst s2 s1)
                    Right (subst, applySubst s3 returnType, c3)
                TVar _ -> do
                    s3 <- unify funcType' (TFunc argType resultType)
                    let subst = composeSubst s3 (composeSubst s2 s1)
                    Right (subst, applySubst s3 resultType, c3)
                other ->
                    Left ("expected function, got " <> show other)
        ListLit elements -> inferList counter ctorEnv env elements
        Nil -> do
            let (elementType, nextCounter) = freshTypeVar counter
            Right ([], TList elementType, nextCounter)
        Cons headExpr tailExpr -> do
            (s1, headType, c1) <- inferWith counter ctorEnv env headExpr
            let env1 = applySubstEnv s1 env
            (s2, tailType, c2) <- inferWith c1 ctorEnv env1 tailExpr
            s3 <- unify (applySubst s2 tailType) (TList headType)
            let subst = composeSubst s3 (composeSubst s2 s1)
            Right (subst, applySubst s3 (TList headType), c2)
        ConstructorExpr name args ->
            inferConstructor counter ctorEnv env name args
        Add left right -> inferIntBinOp counter ctorEnv env left right
        Sub left right -> inferIntBinOp counter ctorEnv env left right
        Mul left right -> inferIntBinOp counter ctorEnv env left right
        GreaterThan left right -> do
            (s1, leftType, c1) <- inferWith counter ctorEnv env left
            let env1 = applySubstEnv s1 env
            (s2, rightType, c2) <- inferWith c1 ctorEnv env1 right
            s3 <- unify (applySubst s2 leftType) TInt
            s4 <- unify (applySubst s3 rightType) TInt
            let subst = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
            Right (subst, TBool, c2)
        If condExpr thenExpr elseExpr -> do
            (s1, condType, c1) <- inferWith counter ctorEnv env condExpr
            s2 <- unify condType TBool
            let env1 = applySubstEnv (composeSubst s2 s1) env
            (s3, thenType, c2) <- inferWith c1 ctorEnv env1 thenExpr
            let env2 = applySubstEnv (composeSubst s3 (composeSubst s2 s1)) env
            (s4, elseType, c3) <- inferWith c2 ctorEnv env2 elseExpr
            s5 <- unify (applySubst s4 thenType) elseType
            let subst = composeSubst s5 (composeSubst s4 (composeSubst s3 (composeSubst s2 s1)))
            Right (subst, applySubst s5 elseType, c3)
        Match scrutinee pattern1 branch1 pattern2 branch2 -> do
            (s1, scrutineeType, c1) <- inferWith counter ctorEnv env scrutinee
            (patType1, patEnv1, c2) <- inferPattern c1 ctorEnv pattern1
            s2 <- unify (applySubst s1 scrutineeType) patType1
                `orElse` matchScrutineeMismatch (applySubst s1 scrutineeType) patType1
            let subst12 = composeSubst s2 s1
            let env1 = applySubstEnv subst12 env
            let patEnv1' = applySubstEnv subst12 patEnv1
            (s3, branchType1, c3) <- inferWith c2 ctorEnv (patEnv1' ++ env1) branch1
            let subst123 = composeSubst s3 subst12
            let env2 = applySubstEnv subst123 env
            (patType2, patEnv2, c4) <- inferPattern c3 ctorEnv pattern2
            s4 <- unify (applySubst subst123 scrutineeType) patType2
                `orElse` matchScrutineeMismatch (applySubst subst123 scrutineeType) patType2
            let subst1234 = composeSubst s4 subst123
            let patEnv2' = applySubstEnv subst1234 patEnv2
            (s5, branchType2, c5) <- inferWith c4 ctorEnv (patEnv2' ++ env2) branch2
            s6 <- unify (applySubst s5 branchType1) branchType2
            let subst = composeSubst s6 (composeSubst s5 subst1234)
            Right (subst, applySubst s6 branchType2, c5)
        Let name valueExpr bodyExpr -> do
            (s1, valueType, c1) <- inferWith counter ctorEnv env valueExpr
            let env1 = applySubstEnv s1 env
            (s2, bodyType, c2) <- inferWith c1 ctorEnv ((name, valueType) : env1) bodyExpr
            Right (composeSubst s2 s1, bodyType, c2)
        LetRec name valueExpr bodyExpr -> do
            let (tempType, c1) = freshTypeVar counter
            let env1 = (name, tempType) : env
            (s1, valueType, c2) <- inferWith c1 ctorEnv env1 valueExpr
            s2 <- unify (applySubst s1 tempType) valueType
            let env2 = applySubstEnv (composeSubst s2 s1) env
            (s3, bodyType, c3) <- inferWith c2 ctorEnv ((name, applySubst s2 valueType) : env2) bodyExpr
            Right (composeSubst s3 (composeSubst s2 s1), bodyType, c3)
        Seq firstExpr secondExpr -> do
            (s1, _, c1) <- inferWith counter ctorEnv env firstExpr
            let env1 = applySubstEnv s1 env
            (s2, secondType, c2) <- inferWith c1 ctorEnv env1 secondExpr
            Right (composeSubst s2 s1, secondType, c2)
        Print expr -> do
            (s1, _, c1) <- inferWith counter ctorEnv env expr
            Right (s1, TUnit, c1)

inferIntBinOp :: Int -> CtorEnv -> TypeEnv -> Expr -> Expr -> Either String (Subst, Type, Int)
inferIntBinOp counter ctorEnv env left right = do
    (s1, leftType, c1) <- inferWith counter ctorEnv env left
    let env1 = applySubstEnv s1 env
    (s2, rightType, c2) <- inferWith c1 ctorEnv env1 right
    s3 <- unify (applySubst s2 leftType) TInt
    s4 <- unify (applySubst s3 rightType) TInt
    let subst = composeSubst s4 (composeSubst s3 (composeSubst s2 s1))
    Right (subst, TInt, c2)

inferList :: Int -> CtorEnv -> TypeEnv -> [Expr] -> Either String (Subst, Type, Int)
inferList counter ctorEnv env elements =
    case elements of
        [] -> do
            let (elementType, nextCounter) = freshTypeVar counter
            Right ([], TList elementType, nextCounter)
        firstElem : rest -> do
            (s1, firstType, c1) <- inferWith counter ctorEnv env firstElem
            let env1 = applySubstEnv s1 env
            (sFinal, _, cFinal) <- foldM (inferListElem env1) (s1, firstType, c1) rest
            Right (sFinal, TList (applySubst sFinal firstType), cFinal)
  where
    inferListElem env1 (sAcc, elementType, cAcc) expr = do
        let env2 = applySubstEnv sAcc env1
        (sNext, nextType, cNext) <- inferWith cAcc ctorEnv env2 expr
        sUnify <- unify (applySubst sNext elementType) nextType
        let subst = composeSubst sUnify (composeSubst sNext sAcc)
        Right (subst, applySubst subst elementType, cNext)

freshTypeVar :: Int -> (Type, Int)
freshTypeVar counter = (TVar ("t" <> show counter), counter + 1)

instantiate :: Int -> Type -> (Type, Int)
instantiate counter ty =
    let (ty', _, nextCounter) = go counter [] ty
    in (ty', nextCounter)
  where
    go c subst current =
        case current of
            TInt -> (TInt, subst, c)
            TBool -> (TBool, subst, c)
            TUnit -> (TUnit, subst, c)
            TList element ->
                let (element', subst', c') = go c subst element
                in (TList element', subst', c')
            TFunc arg result ->
                let (arg', subst', c1) = go c subst arg
                    (result', subst'', c2) = go c1 subst' result
                in (TFunc arg' result', subst'', c2)
            TCon name args ->
                let (args', subst', c') = goList c subst args
                in (TCon name args', subst', c')
            TVar name ->
                case lookup name subst of
                    Just existing -> (existing, subst, c)
                    Nothing ->
                        let (freshVar, c') = freshTypeVar c
                        in (freshVar, (name, freshVar) : subst, c')

    goList c subst args =
        foldl step ([], subst, c) args
      where
        step (acc, substAcc, cAcc) arg =
            let (arg', subst', c') = go cAcc substAcc arg
            in (acc ++ [arg'], subst', c')

builtinNames :: [String]
builtinNames = ["print", "println", "head", "tail", "isEmpty", "length"]

applySubstEnv :: Subst -> TypeEnv -> TypeEnv
applySubstEnv subst = map (\(name, ty) -> (name, applySubst subst ty))

inferPattern :: Int -> CtorEnv -> Pattern -> Either String (Type, TypeEnv, Int)
inferPattern counter ctorEnv pattern =
    case pattern of
        PNil ->
            let (elementType, nextCounter) = freshTypeVar counter
            in Right (TList elementType, [], nextCounter)
        PCons headName tailName ->
            let (elementType, nextCounter) = freshTypeVar counter
                bindings =
                    [ (headName, elementType)
                    , (tailName, TList elementType)
                    ]
            in Right (TList elementType, bindings, nextCounter)
        PCtor name vars ->
            case lookup name ctorEnv of
                Nothing -> Left ("unknown constructor: " <> name)
                Just (params, argTypes, resultType) -> do
                    let (paramSubst, nextCounter) = freshTypeVars params counter
                    let instArgTypes = map (applySubst paramSubst) argTypes
                    let instResultType = applySubst paramSubst resultType
                    if length vars /= length instArgTypes
                        then Left ("constructor arity mismatch for " <> name)
                        else Right (instResultType, zip vars instArgTypes, nextCounter)

unifyTypeLists :: [Type] -> [Type] -> Either String Subst
unifyTypeLists left right =
    foldM unifyStep [] (zip left right)
  where
    unifyStep subst (lTy, rTy) = do
        s1 <- unify (applySubst subst lTy) (applySubst subst rTy)
        Right (composeSubst s1 subst)

inferConstructor :: Int -> CtorEnv -> TypeEnv -> String -> [Expr] -> Either String (Subst, Type, Int)
inferConstructor counter ctorEnv env name args =
    case lookup name ctorEnv of
        Nothing -> Left ("unknown constructor: " <> name)
        Just (params, argTypes, resultType) -> do
            let (paramSubst, nextCounter) = freshTypeVars params counter
            let instArgTypes = map (applySubst paramSubst) argTypes
            let instResultType = applySubst paramSubst resultType
            if length args /= length instArgTypes
                then Left ("constructor arity mismatch for " <> name)
                else inferConstructorArgs name nextCounter ctorEnv env args instArgTypes instResultType

inferConstructorArgs :: String -> Int -> CtorEnv -> TypeEnv -> [Expr] -> [Type] -> Type -> Either String (Subst, Type, Int)
inferConstructorArgs ctorName counter ctorEnv env args expectedArgs resultType =
    case (args, expectedArgs) of
        ([], []) -> Right ([], resultType, counter)
        (argExpr : restExprs, expectedType : restTypes) -> do
            (s1, argType, c1) <- inferWith counter ctorEnv env argExpr
            s2 <- unify argType expectedType
                `orElse` ctorArgMismatch ctorName expectedType argType
            (s3, resType, c2) <- inferConstructorArgs ctorName c1 ctorEnv (applySubstEnv (composeSubst s2 s1) env) restExprs (map (applySubst (composeSubst s2 s1)) restTypes) (applySubst (composeSubst s2 s1) resultType)
            Right (composeSubst s3 (composeSubst s2 s1), resType, c2)
        _ -> Left "constructor arity mismatch"

freshTypeVars :: [String] -> Int -> (Subst, Int)
freshTypeVars names counter =
    foldl build ([], counter) names
  where
    build (subst, c) name =
        let (ty, nextC) = freshTypeVar c
        in ((name, ty) : subst, nextC)

typeMismatch :: Type -> Type -> String
typeMismatch expected actual =
    "type mismatch: expected " <> show expected <> ", got " <> show actual

funcArgMismatch :: Expr -> Type -> Type -> String
funcArgMismatch funcExpr expected actual =
    case funcExpr of
        Var name ->
            "function argument type mismatch for " <> name <> ": expected " <> show expected <> ", got " <> show actual
        _ ->
            "function argument type mismatch: expected " <> show expected <> ", got " <> show actual

ctorArgMismatch :: String -> Type -> Type -> String
ctorArgMismatch ctorName expected actual =
    "constructor argument type mismatch for " <> ctorName <> ": expected " <> show expected <> ", got " <> show actual

matchScrutineeMismatch :: Type -> Type -> String
matchScrutineeMismatch scrutineeType patternType =
    "match scrutinee type mismatch: expected " <> show patternType <> ", got " <> show scrutineeType

orElse :: Either String a -> String -> Either String a
orElse result message =
    case result of
        Left _ -> Left message
        Right value -> Right value
