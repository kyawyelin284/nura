module Bytecode
    ( Instr(..)
    , compile
    ) where

import qualified AST

data Instr
    = PushInt Integer
    | Add
    | Mul
    | Load String
    | Store String
    | Dup
    | MakeClosure Int String Int
    | AllocCons
    | AllocNil
    | AllocCtor String Int
    | LoadField Int
    | MatchNil Int
    | MatchCons Int
    | MatchCtor String Int
    | Jump Int
    | JumpIfFalse Int
    | Call
    | TailCall
    | Return
    | Fail String
    deriving (Show, Eq)

compile :: AST.Expr -> Either String [Instr]
compile expr = do
    (code, _) <- compileWith 0 False expr
    Right code

compileWith :: Int -> Bool -> AST.Expr -> Either String ([Instr], Int)
compileWith base inTail expression =
    case expression of
        AST.IntLit n -> Right ([PushInt n], 1)
        AST.BoolLit True -> Right ([PushInt 1], 1)
        AST.BoolLit False -> Right ([PushInt 0], 1)
        AST.Var name -> Right ([Load name], 1)
        AST.Add left right ->
            compileBin base left right Add
        AST.Mul left right ->
            compileBin base left right Mul
        AST.Lambda param bodyExpr -> do
            let freeVarsList = freeVars bodyExpr [param]
            let captureCode = map Load freeVarsList
            let capLen = length captureCode
            let entryPoint = base + capLen + 2
            let renameMap = zip freeVarsList (map capName [0 :: Int ..])
            let renamedBody = renameFreeVars renameMap bodyExpr [param]
            (bodyCode, bodyLen) <- compileWith entryPoint True renamedBody
            let jumpOffset = bodyLen + 1
            let code =
                    captureCode
                    ++ [MakeClosure entryPoint param capLen]
                    ++ [Jump jumpOffset]
                    ++ bodyCode
                    ++ [Return]
            Right (code, capLen + 2 + bodyLen + 1)
        AST.Apply funcExpr argExpr -> do
            (funcCode, funcLen) <- compileWith base False funcExpr
            (argCode, argLen) <- compileWith (base + funcLen) False argExpr
            let callInstr = if inTail then TailCall else Call
            Right (funcCode ++ argCode ++ [callInstr], funcLen + argLen + 1)
        AST.ListLit elements ->
            compileList base elements
        AST.Nil ->
            Right ([AllocNil], 1)
        AST.Cons headExpr tailExpr -> do
            (headCode, headLen) <- compileWith base False headExpr
            (tailCode, tailLen) <- compileWith (base + headLen) False tailExpr
            Right (headCode ++ tailCode ++ [AllocCons], headLen + tailLen + 1)
        AST.Let name valueExpr bodyExpr ->
            let compiled = do
                    (valueCode, valueLen) <- compileWith base False valueExpr
                    (bodyCode, bodyLen) <- compileWith (base + valueLen + 1) inTail bodyExpr
                    Right (valueCode ++ [Store name] ++ bodyCode, valueLen + 1 + bodyLen)
            in compiled
        AST.If condExpr thenExpr elseExpr ->
            let compiled = do
                    (condCode, condLen) <- compileWith base False condExpr
                    let thenBase = base + condLen + 1
                    (thenCode, thenLen) <- compileWith thenBase inTail thenExpr
                    let elseBase = thenBase + thenLen + 1
                    (elseCode, elseLen) <- compileWith elseBase inTail elseExpr
                    let jumpIfFalseOffset = thenLen + 1
                    let jumpOffset = elseLen
                    let code =
                            condCode
                            ++ [JumpIfFalse jumpIfFalseOffset]
                            ++ thenCode
                            ++ [Jump jumpOffset]
                            ++ elseCode
                    Right (code, condLen + 1 + thenLen + 1 + elseLen)
            in compiled
        AST.Sub _ _ -> Left "subtraction"
        AST.GreaterThan _ _ -> Left "comparison"
        AST.Match scrutinee pattern1 branch1 pattern2 branch2 ->
            compileMatch base scrutinee pattern1 branch1 pattern2 branch2
        AST.LetRec _ _ _ -> Left "recursive let"
        AST.Print _ -> Left "print"
        AST.ConstructorExpr name args ->
            compileConstructor base name args

compileBin :: Int -> AST.Expr -> AST.Expr -> Instr -> Either String ([Instr], Int)
compileBin base left right instr = do
    (leftCode, leftLen) <- compileWith base False left
    (rightCode, rightLen) <- compileWith (base + leftLen) False right
    Right (leftCode ++ rightCode ++ [instr], leftLen + rightLen + 1)

capName :: Int -> String
capName idx = "_cap" <> show idx

freeVars :: AST.Expr -> [String] -> [String]
freeVars expr bound =
    case expr of
        AST.IntLit _ -> []
        AST.BoolLit _ -> []
        AST.Var name -> if name `elem` bound then [] else [name]
        AST.Add left right -> freeVars left bound `uniqueAppend` freeVars right bound
        AST.Mul left right -> freeVars left bound `uniqueAppend` freeVars right bound
        AST.Sub left right -> freeVars left bound `uniqueAppend` freeVars right bound
        AST.GreaterThan left right -> freeVars left bound `uniqueAppend` freeVars right bound
        AST.If condExpr thenExpr elseExpr ->
            freeVars condExpr bound `uniqueAppend`
            freeVars thenExpr bound `uniqueAppend`
            freeVars elseExpr bound
        AST.Let name valueExpr bodyExpr ->
            freeVars valueExpr bound `uniqueAppend` freeVars bodyExpr (name : bound)
        AST.LetRec name valueExpr bodyExpr ->
            freeVars valueExpr (name : bound) `uniqueAppend` freeVars bodyExpr (name : bound)
        AST.Lambda name bodyExpr -> freeVars bodyExpr (name : bound)
        AST.Apply funcExpr argExpr ->
            freeVars funcExpr bound `uniqueAppend` freeVars argExpr bound
        _ -> []

renameFreeVars :: [(String, String)] -> AST.Expr -> [String] -> AST.Expr
renameFreeVars mapping expr bound =
    case expr of
        AST.IntLit _ -> expr
        AST.BoolLit _ -> expr
        AST.Var name ->
            if name `elem` bound
                then expr
                else case lookup name mapping of
                    Just newName -> AST.Var newName
                    Nothing -> expr
        AST.Add left right ->
            AST.Add (renameFreeVars mapping left bound) (renameFreeVars mapping right bound)
        AST.Mul left right ->
            AST.Mul (renameFreeVars mapping left bound) (renameFreeVars mapping right bound)
        AST.Sub left right ->
            AST.Sub (renameFreeVars mapping left bound) (renameFreeVars mapping right bound)
        AST.GreaterThan left right ->
            AST.GreaterThan (renameFreeVars mapping left bound) (renameFreeVars mapping right bound)
        AST.If condExpr thenExpr elseExpr ->
            AST.If
                (renameFreeVars mapping condExpr bound)
                (renameFreeVars mapping thenExpr bound)
                (renameFreeVars mapping elseExpr bound)
        AST.Let name valueExpr bodyExpr ->
            AST.Let
                name
                (renameFreeVars mapping valueExpr bound)
                (renameFreeVars mapping bodyExpr (name : bound))
        AST.LetRec name valueExpr bodyExpr ->
            AST.LetRec
                name
                (renameFreeVars mapping valueExpr (name : bound))
                (renameFreeVars mapping bodyExpr (name : bound))
        AST.Lambda name bodyExpr ->
            AST.Lambda name (renameFreeVars mapping bodyExpr (name : bound))
        AST.Apply funcExpr argExpr ->
            AST.Apply
                (renameFreeVars mapping funcExpr bound)
                (renameFreeVars mapping argExpr bound)
        AST.Match scrutinee pattern1 branch1 pattern2 branch2 ->
            AST.Match
                (renameFreeVars mapping scrutinee bound)
                pattern1
                (renameFreeVars mapping branch1 (patternVars pattern1 ++ bound))
                pattern2
                (renameFreeVars mapping branch2 (patternVars pattern2 ++ bound))
        other -> other

uniqueAppend :: [String] -> [String] -> [String]
uniqueAppend left right =
    foldl addUnique left right
  where
    addUnique acc name =
        if name `elem` acc then acc else acc ++ [name]

compileList :: Int -> [AST.Expr] -> Either String ([Instr], Int)
compileList base elements =
    compileListTail base elements
  where
    compileListTail currentBase elems =
        case elems of
            [] -> Right ([AllocNil], 1)
            headExpr : tailExprs -> do
                (tailCode, tailLen) <- compileListTail currentBase tailExprs
                let headBase = currentBase + tailLen
                (headCode, headLen) <- compileWith headBase False headExpr
                Right (tailCode ++ headCode ++ [AllocCons], tailLen + headLen + 1)

compileConstructor :: Int -> String -> [AST.Expr] -> Either String ([Instr], Int)
compileConstructor base name args = do
    (argCode, argLen) <- compileArgs base args
    Right (argCode ++ [AllocCtor name (length args)], argLen + 1)

compileArgs :: Int -> [AST.Expr] -> Either String ([Instr], Int)
compileArgs base args =
    case args of
        [] -> Right ([], 0)
        firstArg : restArgs -> do
            (firstCode, firstLen) <- compileWith base False firstArg
            (restCode, restLen) <- compileArgs (base + firstLen) restArgs
            Right (firstCode ++ restCode, firstLen + restLen)

compileMatch :: Int -> AST.Expr -> AST.Pattern -> AST.Expr -> AST.Pattern -> AST.Expr -> Either String ([Instr], Int)
compileMatch base scrutinee pattern1 branch1 pattern2 branch2 = do
    (scrutCode, scrutLen) <- compileWith base False scrutinee
    let bind1 = patternBindings pattern1
    let bind1Len = length bind1
    let branch1Base = base + scrutLen + 2 + bind1Len
    (branch1Code, branch1Len) <- compileWith branch1Base True branch1
    let bind2 = patternBindings pattern2
    let bind2Len = length bind2
    let branch2Base = branch1Base + branch1Len + 1 + 1 + bind2Len
    (branch2Code, branch2Len) <- compileWith branch2Base True branch2
    let failLen = 1
    let match1Offset = bind1Len + branch1Len + 1
    let match2Offset = bind2Len + branch2Len + 1
    let jumpOffset = 1 + bind2Len + branch2Len + 1 + failLen
    let code =
            scrutCode
            ++ [Dup]
            ++ [matchInstr pattern1 match1Offset]
            ++ map Store bind1
            ++ branch1Code
            ++ [Jump jumpOffset]
            ++ [matchInstr pattern2 match2Offset]
            ++ map Store bind2
            ++ branch2Code
            ++ [Jump 1]
            ++ [Fail "non-exhaustive match"]
    Right (code, scrutLen + 2 + bind1Len + branch1Len + 1 + 1 + bind2Len + branch2Len + 1 + failLen)

matchInstr :: AST.Pattern -> Int -> Instr
matchInstr pattern offset =
    case pattern of
        AST.PNil -> MatchNil offset
        AST.PCons _ _ -> MatchCons offset
        AST.PCtor name _ -> MatchCtor name offset

patternBindings :: AST.Pattern -> [String]
patternBindings pattern =
    case pattern of
        AST.PNil -> []
        AST.PCons headName tailName -> [headName, tailName]
        AST.PCtor _ vars -> reverse vars

patternVars :: AST.Pattern -> [String]
patternVars pattern =
    case pattern of
        AST.PNil -> []
        AST.PCons headName tailName -> [headName, tailName]
        AST.PCtor _ vars -> vars
