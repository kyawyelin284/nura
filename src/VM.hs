module VM
    ( runVM
    ) where

import Bytecode (Instr(..))

data Value
    = IntVal Integer
    | StringVal String
    | Addr Int
    | BuiltinVal Builtin
    deriving (Show, Eq)

type Stack = [Value]
type Env = [(String, Value)]

data HeapObj
    = ClosureObj Bool Int String Env
    | ListNode Bool Value Value
    | CtorObj Bool String [Value]
    deriving (Show, Eq)

data Builtin
    = BuiltinPrint
    | BuiltinPrintln
    | BuiltinHead
    | BuiltinTail
    | BuiltinIsEmpty
    | BuiltinLength
    | BuiltinStrLen
    deriving (Show, Eq)

data Frame = Frame
    { frameReturnIp :: Int
    , frameEnv :: Env
    } deriving (Show, Eq)

runVM :: [Instr] -> IO ()
runVM instrs = do
    _ <- exec 0 [] (Frame (-1) globalEnv) [] []
    pure ()
  where
    gcThreshold :: Int
    gcThreshold = 1024

    globalEnv :: Env
    globalEnv =
        [ ("print", BuiltinVal BuiltinPrint)
        , ("println", BuiltinVal BuiltinPrintln)
        , ("head", BuiltinVal BuiltinHead)
        , ("tail", BuiltinVal BuiltinTail)
        , ("isEmpty", BuiltinVal BuiltinIsEmpty)
        , ("length", BuiltinVal BuiltinLength)
        , ("strlen", BuiltinVal BuiltinStrLen)
        ]

    maybeCollect :: Stack -> Frame -> [Frame] -> Env -> [HeapObj] -> [HeapObj]
    maybeCollect stack currentFrame callStack globals heap =
        if length heap >= gcThreshold
            then sweepHeap (markFromRoots stack currentFrame callStack globals heap)
            else heap

    exec :: Int -> Stack -> Frame -> [Frame] -> [HeapObj] -> IO (Stack, Frame, [HeapObj])
    exec ip stack currentFrame callStack heap
        | ip < 0 || ip >= length instrs = pure (stack, currentFrame, heap)
        | otherwise =
            case instrs !! ip of
                PushInt n ->
                    exec (ip + 1) (IntVal n : stack) currentFrame callStack heap
                PushString s ->
                    exec (ip + 1) (StringVal s : stack) currentFrame callStack heap
                Add ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (b + a) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (StringVal (b <> a) : rest) currentFrame callStack heap
                        _ -> fail (vmError ip "Add: stack underflow or invalid value types")
                Eq ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (if b == a then 1 else 0) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (IntVal (if b == a then 1 else 0) : rest) currentFrame callStack heap
                        Addr a : Addr b : rest ->
                            case eqValues (Addr a) (Addr b) heap of
                                Just result ->
                                    exec (ip + 1) (IntVal (if result then 1 else 0) : rest) currentFrame callStack heap
                                Nothing ->
                                    fail (vmError ip "Eq: invalid list comparison")
                        _ -> fail (vmError ip "Eq: stack underflow or invalid value types")
                Neq ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (if b /= a then 1 else 0) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (IntVal (if b /= a then 1 else 0) : rest) currentFrame callStack heap
                        Addr a : Addr b : rest ->
                            case eqValues (Addr a) (Addr b) heap of
                                Just result ->
                                    exec (ip + 1) (IntVal (if not result then 1 else 0) : rest) currentFrame callStack heap
                                Nothing ->
                                    fail (vmError ip "Neq: invalid list comparison")
                        _ -> fail (vmError ip "Neq: stack underflow or invalid value types")
                Gt ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (if b > a then 1 else 0) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (IntVal (if b > a then 1 else 0) : rest) currentFrame callStack heap
                        Addr a : Addr b : rest ->
                            case orderValues (Addr b) (Addr a) heap of
                                Just GT ->
                                    exec (ip + 1) (IntVal 1 : rest) currentFrame callStack heap
                                Just _ ->
                                    exec (ip + 1) (IntVal 0 : rest) currentFrame callStack heap
                                Nothing ->
                                    fail (vmError ip "Gt: invalid list comparison")
                        _ -> fail (vmError ip "Gt: stack underflow or invalid value types")
                Lt ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (if b < a then 1 else 0) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (IntVal (if b < a then 1 else 0) : rest) currentFrame callStack heap
                        Addr a : Addr b : rest ->
                            case orderValues (Addr b) (Addr a) heap of
                                Just LT ->
                                    exec (ip + 1) (IntVal 1 : rest) currentFrame callStack heap
                                Just _ ->
                                    exec (ip + 1) (IntVal 0 : rest) currentFrame callStack heap
                                Nothing ->
                                    fail (vmError ip "Lt: invalid list comparison")
                        _ -> fail (vmError ip "Lt: stack underflow or invalid value types")
                Lte ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (if b <= a then 1 else 0) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (IntVal (if b <= a then 1 else 0) : rest) currentFrame callStack heap
                        Addr a : Addr b : rest ->
                            case orderValues (Addr b) (Addr a) heap of
                                Just GT ->
                                    exec (ip + 1) (IntVal 0 : rest) currentFrame callStack heap
                                Just _ ->
                                    exec (ip + 1) (IntVal 1 : rest) currentFrame callStack heap
                                Nothing ->
                                    fail (vmError ip "Lte: invalid list comparison")
                        _ -> fail (vmError ip "Lte: stack underflow or invalid value types")
                Gte ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (if b >= a then 1 else 0) : rest) currentFrame callStack heap
                        StringVal a : StringVal b : rest ->
                            exec (ip + 1) (IntVal (if b >= a then 1 else 0) : rest) currentFrame callStack heap
                        Addr a : Addr b : rest ->
                            case orderValues (Addr b) (Addr a) heap of
                                Just LT ->
                                    exec (ip + 1) (IntVal 0 : rest) currentFrame callStack heap
                                Just _ ->
                                    exec (ip + 1) (IntVal 1 : rest) currentFrame callStack heap
                                Nothing ->
                                    fail (vmError ip "Gte: invalid list comparison")
                        _ -> fail (vmError ip "Gte: stack underflow or invalid value types")
                Mul ->
                    case stack of
                        IntVal a : IntVal b : rest ->
                            exec (ip + 1) (IntVal (b * a) : rest) currentFrame callStack heap
                        _ -> fail (vmError ip "Mul: stack underflow or non-int values")
                Load name ->
                    case lookup name (frameEnv currentFrame) of
                        Just val -> exec (ip + 1) (val : stack) currentFrame callStack heap
                        Nothing -> fail (vmError ip ("Load: unbound variable " <> name))
                Store name ->
                    case stack of
                        val : rest ->
                            let updatedFrame = currentFrame { frameEnv = (name, val) : frameEnv currentFrame }
                            in exec (ip + 1) rest updatedFrame callStack heap
                        _ -> fail (vmError ip "Store: stack underflow")
                Dup ->
                    case stack of
                        val : rest -> exec (ip + 1) (val : val : rest) currentFrame callStack heap
                        _ -> fail (vmError ip "Dup: stack underflow")
                MakeClosure entryPoint paramName captureCount ->
                    case popN captureCount stack of
                        Just (capturedValues, rest) ->
                            let heap' = maybeCollect stack currentFrame callStack globalEnv heap
                                capturedEnv = buildCapturedEnv capturedValues
                                closure = ClosureObj False entryPoint paramName capturedEnv
                                index = length heap'
                            in exec (ip + 1) (Addr index : rest) currentFrame callStack (heap' ++ [closure])
                        Nothing -> fail (vmError ip "MakeClosure: stack underflow")
                AllocNil ->
                    let heap' = maybeCollect stack currentFrame callStack globalEnv heap
                        index = length heap'
                        heapObj = CtorObj False "Nil" []
                    in exec (ip + 1) (Addr index : stack) currentFrame callStack (heap' ++ [heapObj])
                AllocCons ->
                    case stack of
                        headVal : tailVal : rest ->
                            let heap' = maybeCollect stack currentFrame callStack globalEnv heap
                                index = length heap'
                                heapObj = ListNode False headVal tailVal
                            in exec (ip + 1) (Addr index : rest) currentFrame callStack (heap' ++ [heapObj])
                        _ -> fail (vmError ip "AllocCons: stack underflow")
                AllocCtor tag arity ->
                    case popN arity stack of
                        Just (fieldVals, rest) ->
                            let heap' = maybeCollect stack currentFrame callStack globalEnv heap
                                index = length heap'
                                heapObj = CtorObj False tag fieldVals
                            in exec (ip + 1) (Addr index : rest) currentFrame callStack (heap' ++ [heapObj])
                        Nothing -> fail (vmError ip "AllocCtor: stack underflow")
                LoadField index ->
                    case stack of
                        Addr addr : rest ->
                            case heapLookup addr heap of
                                Just (ListNode _ headVal tailVal) ->
                                    case index of
                                        0 -> exec (ip + 1) (headVal : rest) currentFrame callStack heap
                                        1 -> exec (ip + 1) (tailVal : rest) currentFrame callStack heap
                                        _ -> fail (vmError ip "LoadField: list index out of bounds")
                                Just (CtorObj _ _ fields) ->
                                    if index < 0 || index >= length fields
                                        then fail (vmError ip "LoadField: constructor index out of bounds")
                                        else exec (ip + 1) (fields !! index : rest) currentFrame callStack heap
                                Just (ClosureObj _ _ _ _) ->
                                    fail (vmError ip "LoadField: cannot read from closure")
                                Nothing -> fail (vmError ip "LoadField: invalid heap address")
                        _ -> fail (vmError ip "LoadField: expected heap address")
                MatchNil offset ->
                    case stack of
                        Addr addr : rest ->
                            case heapLookup addr heap of
                                Just (CtorObj _ "Nil" []) ->
                                    exec (ip + 1) rest currentFrame callStack heap
                                Just (CtorObj _ tag _) ->
                                    exec (ip + 1 + offset) stack currentFrame callStack heap
                                Just (ListNode _ _ _) ->
                                    fail (vmError ip "MatchNil: expected empty list, got cons cell")
                                Just (ClosureObj _ _ _ _) ->
                                    fail (vmError ip "MatchNil: expected empty list, got closure")
                                Nothing ->
                                    fail (vmError ip "MatchNil: invalid heap address")
                        _ -> fail (vmError ip "MatchNil: expected heap address")
                MatchCons offset ->
                    case stack of
                        Addr addr : rest ->
                            case heapLookup addr heap of
                                Just (ListNode _ headVal tailVal) ->
                                    exec (ip + 1) (headVal : tailVal : rest) currentFrame callStack heap
                                Just (CtorObj _ tag _) ->
                                    fail (vmError ip ("MatchCons: expected list, got constructor " <> tag))
                                Just (ClosureObj _ _ _ _) ->
                                    fail (vmError ip "MatchCons: expected list, got closure")
                                Nothing ->
                                    fail (vmError ip "MatchCons: invalid heap address")
                        _ -> fail (vmError ip "MatchCons: expected heap address")
                MatchCtor tag offset ->
                    case stack of
                        Addr addr : rest ->
                            case heapLookup addr heap of
                                Just (CtorObj _ ctorTag fields)
                                    | ctorTag == tag ->
                                        let pushed = reverse fields ++ rest
                                        in exec (ip + 1) pushed currentFrame callStack heap
                                    | otherwise ->
                                        exec (ip + 1 + offset) stack currentFrame callStack heap
                                Just (ListNode _ _ _) ->
                                    fail (vmError ip ("MatchCtor: expected constructor " <> tag <> ", got list"))
                                Just (ClosureObj _ _ _ _) ->
                                    fail (vmError ip ("MatchCtor: expected constructor " <> tag <> ", got closure"))
                                Nothing ->
                                    fail (vmError ip ("MatchCtor: invalid heap address for " <> tag))
                        _ -> fail (vmError ip "MatchCtor: expected heap address")
                Jump offset ->
                    exec (ip + 1 + offset) stack currentFrame callStack heap
                JumpIfFalse offset ->
                    case stack of
                        IntVal val : rest ->
                            if val == 0
                                then exec (ip + 1 + offset) rest currentFrame callStack heap
                                else exec (ip + 1) rest currentFrame callStack heap
                        _ -> fail (vmError ip "JumpIfFalse: stack underflow or non-int value")
                Call ->
                    case stack of
                        argValue : Addr index : rest ->
                            case heapLookup index heap of
                                Just (ClosureObj _ entryPoint paramName capturedEnv) ->
                                    let returnFrame = currentFrame { frameReturnIp = ip + 1 }
                                        newEnv = (paramName, argValue) : capturedEnv
                                    in exec entryPoint rest (Frame (-1) newEnv) (returnFrame : callStack) heap
                                Just (CtorObj _ tag _) ->
                                    fail (vmError ip ("Call: expected closure, got constructor " <> tag))
                                Just (ListNode _ _ _) ->
                                    fail (vmError ip "Call: expected closure, got list")
                                Nothing ->
                                    fail (vmError ip "Call: invalid closure reference")
                        argValue : BuiltinVal builtin : rest -> do
                            result <- applyBuiltin builtin argValue heap ip
                            exec (ip + 1) (result : rest) currentFrame callStack heap
                        _ -> fail (vmError ip "Call: expected [... closure argument]")
                TailCall ->
                    case stack of
                        argValue : Addr index : rest ->
                            case heapLookup index heap of
                                Just (ClosureObj _ entryPoint paramName capturedEnv) ->
                                    let newEnv = (paramName, argValue) : capturedEnv
                                    in exec entryPoint rest (Frame (-1) newEnv) callStack heap
                                Just (CtorObj _ tag _) ->
                                    fail (vmError ip ("TailCall: expected closure, got constructor " <> tag))
                                Just (ListNode _ _ _) ->
                                    fail (vmError ip "TailCall: expected closure, got list")
                                Nothing ->
                                    fail (vmError ip "TailCall: invalid closure reference")
                        argValue : BuiltinVal builtin : rest -> do
                            result <- applyBuiltin builtin argValue heap ip
                            exec (ip + 1) (result : rest) currentFrame callStack heap
                        _ -> fail (vmError ip "TailCall: expected [... closure argument]")
                Return ->
                    case callStack of
                        Frame returnIp returnEnv : rest ->
                            case stack of
                                result : restStack ->
                                    exec returnIp (result : restStack) (Frame (-1) returnEnv) rest heap
                                _ -> fail (vmError ip "Return: stack underflow")
                        [] -> pure (stack, currentFrame, heap)
                Fail message ->
                    fail (vmError ip ("Fail: " <> message))

    popN :: Int -> [a] -> Maybe ([a], [a])
    popN n xs
        | n < 0 = Nothing
        | otherwise = go n xs []
      where
        go 0 rest acc = Just (reverse acc, rest)
        go _ [] _ = Nothing
        go k (y : ys) acc = go (k - 1) ys (y : acc)

    buildCapturedEnv :: [Value] -> Env
    buildCapturedEnv values =
        zipWith (\idx val -> ("_cap" <> show idx, val)) [0 :: Int ..] values

    heapLookup :: Int -> [HeapObj] -> Maybe HeapObj
    heapLookup index heap
        | index < 0 || index >= length heap = Nothing
        | otherwise = Just (heap !! index)

    vmError :: Int -> String -> String
    vmError ip message =
        "VM error at IP " <> show ip <> ": " <> message

    markFromRoots :: Stack -> Frame -> [Frame] -> Env -> [HeapObj] -> [HeapObj]
    markFromRoots stack currentFrame callStack globals heap =
        let roots = stackValues stack
                ++ envValues (frameEnv currentFrame)
                ++ concatMap (envValues . frameEnv) callStack
                ++ envValues globals
        in foldl (flip markValue) heap roots

    sweepHeap :: [HeapObj] -> [HeapObj]
    sweepHeap heap =
        map resetMark (filter isMarked heap)

    isMarked :: HeapObj -> Bool
    isMarked obj =
        case obj of
            ClosureObj marked _ _ _ -> marked
            ListNode marked _ _ -> marked
            CtorObj marked _ _ -> marked

    resetMark :: HeapObj -> HeapObj
    resetMark obj =
        case obj of
            ClosureObj _ entry param env -> ClosureObj False entry param env
            ListNode _ headVal tailVal -> ListNode False headVal tailVal
            CtorObj _ tag fields -> CtorObj False tag fields

    stackValues :: Stack -> [Value]
    stackValues = id

    envValues :: Env -> [Value]
    envValues = map snd

    markValue :: Value -> [HeapObj] -> [HeapObj]
    markValue value heap =
        case value of
            IntVal _ -> heap
            StringVal _ -> heap
            Addr addr -> markHeapObject addr heap
            BuiltinVal _ -> heap

    markHeapObject :: Int -> [HeapObj] -> [HeapObj]
    markHeapObject addr heap =
        case heapLookup addr heap of
            Just (ClosureObj True _ _ _) -> heap
            Just (ListNode True _ _) -> heap
            Just (CtorObj True _ _) -> heap
            Just (ClosureObj False entry param env) ->
                let heap' = updateHeap addr (ClosureObj True entry param env) heap
                in foldl (flip markValue) heap' (envValues env)
            Just (ListNode False headVal tailVal) ->
                let heap' = updateHeap addr (ListNode True headVal tailVal) heap
                in foldl (flip markValue) heap' [headVal, tailVal]
            Just (CtorObj False tag fields) ->
                let heap' = updateHeap addr (CtorObj True tag fields) heap
                in foldl (flip markValue) heap' fields
            Nothing -> heap

    updateHeap :: Int -> HeapObj -> [HeapObj] -> [HeapObj]
    updateHeap addr newObj heap =
        take addr heap ++ [newObj] ++ drop (addr + 1) heap

    applyBuiltin :: Builtin -> Value -> [HeapObj] -> Int -> IO Value
    applyBuiltin builtin value heap ip =
        case builtin of
            BuiltinPrint -> do
                putStrLn (renderValue value heap)
                pure (IntVal 0)
            BuiltinPrintln -> do
                putStrLn (renderValue value heap)
                pure (IntVal 0)
            BuiltinHead ->
                case value of
                    Addr addr ->
                        case heapLookup addr heap of
                            Just (ListNode _ headVal _) -> pure headVal
                            Just (CtorObj _ "Nil" []) -> fail (vmError ip "head: empty list")
                            Just (CtorObj _ tag _) -> fail (vmError ip ("head: expected list, got constructor " <> tag))
                            Just (ClosureObj _ _ _ _) -> fail (vmError ip "head: expected list, got closure")
                            Nothing -> fail (vmError ip "head: invalid heap address")
                    _ -> fail (vmError ip "head: expected list address")
            BuiltinTail ->
                case value of
                    Addr addr ->
                        case heapLookup addr heap of
                            Just (ListNode _ _ tailVal) -> pure tailVal
                            Just (CtorObj _ "Nil" []) -> fail (vmError ip "tail: empty list")
                            Just (CtorObj _ tag _) -> fail (vmError ip ("tail: expected list, got constructor " <> tag))
                            Just (ClosureObj _ _ _ _) -> fail (vmError ip "tail: expected list, got closure")
                            Nothing -> fail (vmError ip "tail: invalid heap address")
                    _ -> fail (vmError ip "tail: expected list address")
            BuiltinIsEmpty ->
                case value of
                    Addr addr ->
                        case heapLookup addr heap of
                            Just (CtorObj _ "Nil" []) -> pure (IntVal 1)
                            Just (ListNode _ _ _) -> pure (IntVal 0)
                            Just (CtorObj _ _ _) -> pure (IntVal 0)
                            Just (ClosureObj _ _ _ _) -> fail (vmError ip "isEmpty: expected list, got closure")
                            Nothing -> fail (vmError ip "isEmpty: invalid heap address")
                    _ -> fail (vmError ip "isEmpty: expected list address")
            BuiltinLength ->
                case value of
                    Addr addr -> IntVal <$> listLength addr heap ip
                    _ -> fail (vmError ip "length: expected list address")
            BuiltinStrLen ->
                case value of
                    StringVal s -> pure (IntVal (fromIntegral (length s)))
                    _ -> fail (vmError ip "strlen: expected string")

    listLength :: Int -> [HeapObj] -> Int -> IO Integer
    listLength addr heap ip =
        case heapLookup addr heap of
            Just (CtorObj _ "Nil" []) -> pure 0
            Just (ListNode _ _ tailVal) ->
                case tailVal of
                    Addr tailAddr -> do
                        rest <- listLength tailAddr heap ip
                        pure (1 + rest)
                    _ -> fail (vmError ip "length: list tail is not an address")
            Just (CtorObj _ tag _) -> fail (vmError ip ("length: expected list, got constructor " <> tag))
            Just (ClosureObj _ _ _ _) -> fail (vmError ip "length: expected list, got closure")
            Nothing -> fail (vmError ip "length: invalid heap address")

    eqValues :: Value -> Value -> [HeapObj] -> Maybe Bool
    eqValues leftVal rightVal heap =
        case (leftVal, rightVal) of
            (IntVal l, IntVal r) -> Just (l == r)
            (StringVal l, StringVal r) -> Just (l == r)
            (Addr leftAddr, Addr rightAddr) -> eqListAddrs leftAddr rightAddr heap
            _ -> Nothing

    eqListAddrs :: Int -> Int -> [HeapObj] -> Maybe Bool
    eqListAddrs leftAddr rightAddr heap
        | leftAddr == rightAddr = Just True
        | otherwise =
            case (heapLookup leftAddr heap, heapLookup rightAddr heap) of
                (Just (CtorObj _ "Nil" []), Just (CtorObj _ "Nil" [])) -> Just True
                (Just (ListNode _ leftHead leftTail), Just (ListNode _ rightHead rightTail)) -> do
                    headsEqual <- eqValues leftHead rightHead heap
                    tailsEqual <- eqValues leftTail rightTail heap
                    Just (headsEqual && tailsEqual)
                (Just (CtorObj _ "Nil" []), Just (ListNode _ _ _)) -> Just False
                (Just (ListNode _ _ _), Just (CtorObj _ "Nil" [])) -> Just False
                _ -> Nothing

    orderValues :: Value -> Value -> [HeapObj] -> Maybe Ordering
    orderValues leftVal rightVal heap =
        case (leftVal, rightVal) of
            (IntVal l, IntVal r) -> Just (compare l r)
            (StringVal l, StringVal r) -> Just (compare l r)
            (Addr leftAddr, Addr rightAddr) ->
                case (heapLookup leftAddr heap, heapLookup rightAddr heap) of
                    (Just (ListNode _ _ _), _) -> compareListAddrs leftAddr rightAddr heap
                    (Just (CtorObj _ "Nil" []), _) -> compareListAddrs leftAddr rightAddr heap
                    (Just (CtorObj _ _ _), Just (CtorObj _ _ _)) -> compareCtorAddrs leftAddr rightAddr heap
                    _ -> Nothing
            _ -> Nothing

    compareListAddrs :: Int -> Int -> [HeapObj] -> Maybe Ordering
    compareListAddrs leftAddr rightAddr heap
        | leftAddr == rightAddr = Just EQ
        | otherwise =
            case (heapLookup leftAddr heap, heapLookup rightAddr heap) of
                (Just (CtorObj _ "Nil" []), Just (CtorObj _ "Nil" [])) -> Just EQ
                (Just (CtorObj _ "Nil" []), Just (ListNode _ _ _)) -> Just LT
                (Just (ListNode _ _ _), Just (CtorObj _ "Nil" [])) -> Just GT
                (Just (ListNode _ leftHead leftTail), Just (ListNode _ rightHead rightTail)) -> do
                    headOrder <- orderValues leftHead rightHead heap
                    case headOrder of
                        EQ -> orderValues leftTail rightTail heap
                        _ -> Just headOrder
                _ -> Nothing

    compareCtorAddrs :: Int -> Int -> [HeapObj] -> Maybe Ordering
    compareCtorAddrs leftAddr rightAddr heap
        | leftAddr == rightAddr = Just EQ
        | otherwise =
            case (heapLookup leftAddr heap, heapLookup rightAddr heap) of
                (Just (CtorObj _ leftTag leftFields), Just (CtorObj _ rightTag rightFields)) ->
                    case compare leftTag rightTag of
                        EQ -> compareValueLists leftFields rightFields heap
                        other -> Just other
                _ -> Nothing

    compareValueLists :: [Value] -> [Value] -> [HeapObj] -> Maybe Ordering
    compareValueLists left right heap =
        case (left, right) of
            ([], []) -> Just EQ
            ([], _) -> Just LT
            (_, []) -> Just GT
            (l : ls, r : rs) -> do
                headOrder <- orderValues l r heap
                case headOrder of
                    EQ -> compareValueLists ls rs heap
                    _ -> Just headOrder

    renderValue :: Value -> [HeapObj] -> String
    renderValue value heap =
        case value of
            IntVal n -> show n
            StringVal s -> s
            Addr addr ->
                case heapLookup addr heap of
                    Just (CtorObj _ "Nil" []) -> "[]"
                    Just (ListNode _ _ _) -> "[" <> renderList addr heap <> "]"
                    Just (CtorObj _ tag fields) ->
                        tag <> "(" <> renderFields fields heap <> ")"
                    Just (ClosureObj _ _ _ _) -> "<closure>"
                    Nothing -> "<invalid>"
            BuiltinVal _ -> "<builtin>"

    renderFields :: [Value] -> [HeapObj] -> String
    renderFields fields heap =
        case fields of
            [] -> ""
            [v] -> renderValue v heap
            v : rest -> renderValue v heap <> ", " <> renderFields rest heap

    renderList :: Int -> [HeapObj] -> String
    renderList addr heap =
        case heapLookup addr heap of
            Just (CtorObj _ "Nil" []) -> ""
            Just (ListNode _ headVal tailVal) ->
                let headText = renderValue headVal heap
                in case tailVal of
                    Addr tailAddr ->
                        case heapLookup tailAddr heap of
                            Just (CtorObj _ "Nil" []) -> headText
                            _ -> headText <> ", " <> renderList tailAddr heap
                    _ -> headText
            _ -> ""

