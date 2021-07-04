module Logic where
import AbsBing
    ( Ident(..),
      RelOp(..),
      MulOp(..),
      AddOp(..),
      Expr(..),
      Type(Void),
      ECond(..),
      Item(..),
      Stmt(..),
      Block(..),
      Param(..),
      Arg(..),
      TopDef(..),
      Program(..) )
import qualified AbsBing as Abs
import Control.Monad.State
    ( foldM, modify, MonadState(get), MonadTrans(lift), StateT, execStateT )
import Control.Monad.Reader ( MonadReader(local, ask), ReaderT (runReaderT) )
import Control.Monad.Except ( MonadError(throwError), ExceptT )
import Data.Map ( (!), empty, findMax, insert, lookup, size, Map, member, filter, delete, filterWithKey )
import Data.Bifunctor ( Bifunctor(first) )
import Control.Monad.Identity ( foldM )
import Commons (BlockDepth, Verbosity)
import qualified Control.Monad
import qualified Data.List


type Loc = Int
type InterpreterOptions = Verbosity

data VarInfo = VarInfo {location :: Loc, depth :: BlockDepth, is_const :: Bool}
        deriving Show

type State = Map Loc Value
type Env = Map Ident VarInfo -- Location and is defined in current env
type FunDef = (Type, [Param], Block)

data Value = Int Integer
    | Bool Bool
    | String String
    | Fun FunDef


type Context = (Env, BlockDepth, Mode)

data Mode = Normal (Maybe Value)
    | Return (Maybe Value)
    | BreakLoop
    | ContinueLoop
    deriving Show

type Interpreter m = StateT (State, InterpreterOptions) (ReaderT Context (ExceptT String IO)) m


instance Show Value where
    show v = case v of
        Int i -> show i
        Bool b -> show b
        String s -> s
        Fun (funtype, funargs, funblock) -> "Fun: " ++ show funargs ++ "" ++ show funtype

absValueType :: Value -> Abs.Type
absValueType v = case v of
    (Int _) -> Abs.Int
    (Bool _) -> Abs.Bool
    (String _) -> Abs.Str
    (Fun (dtype, params, block)) -> Abs.Fun dtype (map (\(Param ptype _) -> ptype) params)  -- Feature - passing function as params

compareValueTypes :: Value -> Value -> Bool
compareValueTypes v1 v2 = absValueType v1 == absValueType v2

isFun :: Value -> Bool
isFun (Fun _) = True
isFun _ = False

debugPrint :: String -> Interpreter ()
debugPrint s = do
    (_, options) <- get
    Control.Monad.when (options > 0) $
            lift $ lift $ lift $ putStrLn s

retrieveValue :: Ident -> Interpreter Value
retrieveValue ident = do
    (store, _) <- get
    (env, _, _) <- ask
    if member ident env
        then do
            let loc = location $ env ! ident
            return (store ! loc)
        else throwError $ "Cannot retrieve value " ++ show ident ++"- not found in environment: " ++ show env

getLoc :: Interpreter Loc
getLoc = do
    (store, _) <- get
    let loc = if size store /= 0 then fst (findMax store) + 1 else 1
    return loc

declVariable :: Bool -> Type -> Ident -> Value -> Interpreter Context
declVariable is_const dtype ident value = do
    (env, block_depth, _) <- ask
    if absValueType value == dtype 
        then 
            case Data.Map.lookup ident env of
                Just (VarInfo loc var_depth is_const) -> do
                    if block_depth == var_depth
                        then throwError $ show ident ++ " is already declared in current block!"
                    else
                        declare
                Nothing -> declare
        else 
            throwError $ "Declared type " ++ show dtype ++ " does not correspond value: " ++ show value
            where
                declare :: Interpreter Context
                declare = do
                    (env, block_depth, _) <- ask
                    new_loc <- getLoc
                    let new_state = insert ident (VarInfo new_loc block_depth is_const) env
                    modify (Data.Bifunctor.first (insert new_loc value))
                    return (new_state, block_depth, Normal Nothing)

declReuseLocation :: Ident -> Ident -> Interpreter Context
declReuseLocation root_ident new_ident = do
    (env, block_depth, _) <- ask
    case Data.Map.lookup root_ident env of
        Just (VarInfo loc var_depth is_const) -> do
            if block_depth == var_depth
                then throwError $ show new_ident ++ " is already declared in current block!"
            else do
                let new_state = insert new_ident (VarInfo loc block_depth is_const) env
                return (new_state, block_depth, Normal Nothing)
        Nothing -> throwError $ "Cannot pass the location of variable: " ++ show root_ident ++ " because it does not exist"


declFun :: TopDef -> Interpreter Context
declFun (FnDef rettype ident params block) = do
    declVariable True (Abs.Fun rettype (map (\(Param ptype _) -> ptype) params)) ident (Fun (rettype, params, block))


assign :: Bool -> Ident -> Expr -> Interpreter Context
assign override_const ident expr = do
    state <- get
    (env, block_depth, _) <- ask
    case Data.Map.lookup ident env of
        Just (VarInfo loc _ is_const) -> do
            if is_const && not override_const
                then
                    throwError $ "Attempt to write to const variable: " ++ show ident
                else do
                    mvalue <- evalExpr expr
                    value <- unpackThrow mvalue
                    (state, _) <- get
                    let prev_value = state ! loc
                    if compareValueTypes value prev_value
                        then do
                            modify (Data.Bifunctor.first (insert loc value))
                            ask
                        else
                            throwError $ "Different type, actual value: " ++ show prev_value ++ " new: " ++ show value
        _ -> throwError $ "Undeclared variable: " ++ show ident

defaultValue :: Type -> Interpreter Value
defaultValue dtype = case dtype of
    Abs.Int -> return $ Int 0
    Abs.Bool -> return $ Bool False
    Abs.Str -> return $ String ""
    _ -> throwError $ "No default value for type: " ++ show dtype


declItem :: Bool -> Type -> Item -> Interpreter Context
declItem is_const dtype item = do
    case item of
        NoInit ident -> do
            value <- defaultValue dtype
            declVariable is_const dtype ident value
        Init ident expr -> do
            mvalue <- evalExpr expr
            value <- unpackThrow mvalue
            declVariable is_const dtype ident value

declItems :: Bool -> Type -> [Item] -> Interpreter Context
declItems is_const dtype l = do
    ctx <- ask
    foldM (\ctx item -> local (const ctx) (declItem is_const dtype item)) ctx l


evalMulOp :: MulOp -> Integer -> Integer -> Interpreter Integer
evalMulOp mulOp i1 i2 = case mulOp of
    Times -> return (i1 * i2)
    Div -> if i2 == 0
        then
            throwError $ "Division by 0!" ++ show i1 ++ " / " ++ show i2
        else
            return (div i1 i2)
    Mod -> if i2 == 0
        then
            throwError $ "Division by 0!" ++ show i1 ++ " % " ++ show i2
        else
            return (mod i1 i2)

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp addOp i1 i2
    | addOp == Plus = i1 + i2
    | addOp == Minus = i1 - i2

evalRelOp :: RelOp -> Integer -> Integer -> Bool
evalRelOp relOp = case relOp of
            EQU -> (==)
            NE -> (/=)
            LE -> (<=)
            LTH -> (<)
            GE -> (>=)
            GTH -> (>)

unpackThrow :: Maybe Value -> Interpreter Value
unpackThrow Nothing = throwError "Function does not return anything (result expected)!"
unpackThrow (Just x) = return x

evalExpr :: Expr -> Interpreter (Maybe Value)
evalExpr op = case op of
    EVar ident -> do
        v <- retrieveValue ident
        return (Just v) -- Ensure that fun will raise an error
    ELitInt v -> return (Just (Int v))
    ELitTrue -> return (Just (Bool True)) -- Consider BExp
    ELitFalse -> return (Just (Bool True))
    EApp ident exprs -> runFun ident exprs

    EString s -> return (Just (String s))
    Not expr -> do
        mres <- evalExpr expr
        res <- unpackThrow mres
        case res of
            (Bool b) -> return (Just (Bool (not b)))
            _ -> throwError $ "Unexpected type in the use of not: " ++ show expr
    Neg expr -> do
        mres <- evalExpr expr
        res <- unpackThrow mres
        case res of
            (Int i) -> return (Just (Int (negate i)))
            _ -> throwError $ "Unexpected type in the use of neg: " ++ show expr

    EAdd v1 addop v2 -> do
        mr1 <- evalExpr v1
        mr2 <- evalExpr v2
        r1 <- unpackThrow mr1
        r2 <- unpackThrow mr2
        case (r1, r2) of
            (Int i1, Int i2) -> return (Just (Int (evalAddOp addop i1 i2)))
            _ -> throwError $ "Unexpected type in: " ++ show v1 ++ show addop ++ show v2

    EMul v1 mulop v2 -> do
        mr1 <- evalExpr v1
        mr2 <- evalExpr v2
        r1 <- unpackThrow mr1
        r2 <- unpackThrow mr2
        case (r1, r2) of
            (Int i1, Int i2) -> do
                result <- evalMulOp mulop i1 i2
                return (Just (Int result))
            _ -> throwError $ "Unexpected type in: " ++ show v1 ++ show mulop ++ show v2

    ERel v1 relop v2 -> do
        mr1 <- evalExpr v1
        mr2 <- evalExpr v2
        r1 <- unpackThrow mr1
        r2 <- unpackThrow mr2
        case (r1, r2) of
            (Int i1, Int i2) -> return (Just (Bool (evalRelOp relop i1 i2)))
            _ -> throwError $ "Unexpected type in: " ++ show v1 ++ show relop ++ show v2

    EAnd v1 v2 -> do
        mr1 <- evalExpr v1
        mr2 <- evalExpr v2
        r1 <- unpackThrow mr1
        r2 <- unpackThrow mr2
        case (r1, r2) of
            (Bool b1, Bool b2) -> return (Just (Bool (b1 && b2)))
            _ -> throwError $ "Unexpected type in: " ++ show v1 ++ " AND " ++ show v2

    EOr v1 v2 -> do
        mr1 <- evalExpr v1
        mr2 <- evalExpr v2
        r1 <- unpackThrow mr1
        r2 <- unpackThrow mr2
        case (r1, r2) of
            (Bool b1, Bool b2) -> return (Just (Bool (b1 || b2)))
            _ -> throwError $ "Unexpected type in: " ++ show v1 ++ " OR " ++ show v2


evalECond :: ECond -> Interpreter Context
evalECond cond = case cond of
    ECondS expr block -> do
        b <- isTrueExpr expr
        if b
            then
                evalBlock block
            else
                ask
    ECondC expr block econd -> do
        b <- isTrueExpr expr
        if b
            then
                evalBlock block
            else
                evalECond econd
    ECondE expr block blockelse -> do
        b <- isTrueExpr expr
        if b
            then
                evalBlock block
            else
                evalBlock blockelse

isTrueExpr :: Expr -> Interpreter Bool
isTrueExpr expr = do
    mr <- evalExpr expr
    r <- unpackThrow mr
    case r of
        Bool b -> return b
        _ -> throwError $ "Expr does not evaluate to bool: " ++ show expr

runBlock :: Block -> Interpreter Context
runBlock (Block (h:t)) = do
    context <- ask
    (env, depth, mode) <- ask
    case mode of
        Normal _ -> do
            state <- get
            debugPrint $ "Running: " ++ show h
            debugPrint $ "PREV CONTEXT: " ++ show context

            new_context <- evalStmt h
            debugPrint $ "NEW CONTEXT: " ++ show new_context
            local (const new_context) (runBlock (Block t))
        BreakLoop -> 
            return (env, depth, BreakLoop)
        ContinueLoop -> 
            return (env, depth, ContinueLoop)
        Return x ->
            return (env, depth, Normal x)
runBlock (Block []) = ask


evalBlock :: Block -> Interpreter Context
evalBlock block = do
    (env, depth, mode) <- ask
    (_, _, new_mode) <- local (const (env, depth + 1, mode)) (runBlock block)
    return (env, depth, new_mode)

evalStmt :: Stmt -> Interpreter Context
evalStmt stmt = case stmt of
    Empty -> ask
    BStmt block -> evalBlock block
    Decl dtype items -> do
        declItems False dtype items
    FDecl topdef -> do
        declFun topdef

    CDecl dtype items -> do
        declItems True dtype items

    Ass ident expr -> assign False ident expr

    Incr ident -> evalStmt $ Ass ident (EAdd (EVar ident) Plus (ELitInt 1))
    Decr ident -> evalStmt $ Ass ident (EAdd (EVar ident) Minus (ELitInt 1))
    VRet -> do
        (env, block_depth, _) <- ask
        return (env, block_depth, Return Nothing)
    Ret expr -> do
        (env, block_depth, _) <- ask
        mvalue <- evalExpr expr
        value <- unpackThrow mvalue
        return (env, block_depth, Return $ Just value)
    While expr stmt -> do
        b <- isTrueExpr expr
        if b
            then do
                (env, depth, mode) <- evalStmt stmt
                case mode of
                    (Return _) -> return (env, depth, mode)
                    (Normal _) -> local (const (env, depth, mode)) $ evalStmt (While expr stmt)
                    ContinueLoop -> local (const (env, depth, Normal Nothing)) $ evalStmt (While expr stmt)
                    BreakLoop -> return (env, depth, Normal Nothing)
            else
                ask
    For ident expStart expStop stmt -> do
        (env, depth, _) <- ask
        let iterVar = Init ident expStart
        context <- declItem True Abs.Int iterVar
        mvalueStop <- evalExpr expStop
        valueStop <- unpackThrow mvalueStop
        case valueStop of
            (Int x) -> do
                let loopExpr = ERel (EVar ident) LTH (ELitInt x)
                (_, _, new_mode) <- local (const context) (iterFor ident loopExpr stmt)
                return (env, depth, new_mode)
            _ -> throwError $ "Exp " ++ show expStop ++ " does not evaluate to int"

        where
            iterFor :: Ident -> Expr -> Stmt -> Interpreter Context
            iterFor ident expr stmt = do
                loop_cond <- isTrueExpr expr
                if loop_cond
                    then do
                        stmt_context <- evalStmt stmt
                        next_context <- local (const stmt_context) (assign True ident (EAdd (EVar ident) Plus (ELitInt 1)))
                        let (env, depth, mode) = stmt_context
                        case mode of
                            (Return _) -> return stmt_context
                            (Normal _) -> local (const next_context) (iterFor ident expr stmt)
                            ContinueLoop -> local (const (env, depth, Normal Nothing)) (iterFor ident expr stmt)
                            BreakLoop -> return (env, depth, Normal Nothing)
                    else
                        ask
    SExp expr -> do
        (env, block_depth, _) <- ask
        mr <- evalExpr expr
        return (env, block_depth, Normal mr)
    Println expr -> do
        mr <- evalExpr expr
        r <- unpackThrow mr
        debugPrint $ "printing " ++ show r
        lift $ lift $ lift $ print r
        ask
    Print expr -> do
        mr <- evalExpr expr
        r <- unpackThrow mr
        debugPrint $ "printing " ++ show r
        lift $ lift $ lift $ putStr $ show r
        ask
    CondS expr block -> do
        r <- isTrueExpr expr
        if r
            then
                evalBlock block
            else
                ask
    CondE expr block blockelse -> do
        r <- isTrueExpr expr
        evalBlock (if r then block else blockelse)
    CondC expr block econd -> do
        r <- isTrueExpr expr
        if r
            then
                evalBlock block
            else
                evalECond econd
    Continue -> do
        (env, depth, mode) <- ask
        return (env, depth, ContinueLoop)
    Break -> do
        (env, depth, mode) <- ask
        return (env, depth, BreakLoop)

runFun :: Ident -> [Arg] -> Interpreter (Maybe Value)
runFun fun_ident args = do
    maybe_fun <- retrieveValue fun_ident
    case maybe_fun of
        (Fun fun) -> do
            let (func_rettype, params, block) = fun

            (env, depth, _) <- ask
            let new_context = (env, depth + 1, Normal Nothing)
            (enriched_env, _, _) <- foldM (\ctx (param, arg) -> local (const ctx) (declOrPass param arg)) new_context (zip params args)

            (state, _) <- get
            -- Remove previous context parameters
            let desired_env = Data.Map.filterWithKey (\arg_ident value -> (isFun $ state ! location value) || (Data.List.any (\(Param _ param_ident) -> arg_ident == param_ident) params)) enriched_env
            let fun_context = (desired_env, depth + 1, Normal Nothing)
            (_, _, mode) <- local (const fun_context) (evalBlock block)

            case (func_rettype, mode) of
                (_, BreakLoop) -> throwError $ "Function " ++ show fun_ident ++ " has break command in it out of loop."
                (_, ContinueLoop) -> throwError $ "Function " ++ show fun_ident ++ " has continue command in it out of loop."
                (Void, _) -> return Nothing
                (_, Return (Just retval)) -> do
                    let rettype = absValueType retval
                    if func_rettype == rettype
                        then 
                            return (Just retval)
                        else 
                            throwError $ "Function " ++ show fun_ident ++ " returned type: " ++ show rettype ++ " when expected type: " ++ show func_rettype 
                (_, Return Nothing) -> return Nothing
                _ -> throwError $ "Function " ++ show fun_ident ++ " did not return any value!"

        _ -> throwError $ show fun_ident ++ " is not a function!"

    where
        declOrPass :: Param -> Arg -> Interpreter Context
        declOrPass (Param param_type param_ident) (VArg expr) = do
            mvalue <- evalExpr expr
            value <- unpackThrow mvalue
            declVariable False param_type param_ident value
        declOrPass (Param param_type param_ident) (PArg ident) = do 
            value <- retrieveValue ident
            let argtype = absValueType value
            if argtype == param_type
                then
                    declReuseLocation ident param_ident
                else
                    throwError $ "Passed variable has another type: " ++ show argtype ++ ", expected: " ++ show param_type  


evalProgram :: Program -> Interpreter ()
evalProgram (Program funcdecls) = do
    let empty_context = (Data.Map.empty, 0, Normal Nothing)
    context <- foldM (\ctx funcdecl -> local (const ctx) (declFun funcdecl)) empty_context funcdecls
    debugPrint $ "Parsed functions: " ++ show context
    local (const context) (runFun (Ident "main") [])
    return ()

interpret :: Verbosity -> Program -> ExceptT String IO ()
interpret verbosity p = do
    runReaderT (execStateT (evalProgram p) (empty, verbosity)) (empty, 0, Normal Nothing)
    return ()

    