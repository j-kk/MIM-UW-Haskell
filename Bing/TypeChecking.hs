module TypeChecking where
import AbsBing
    ( Ident(..),
      Expr(..),
      Arg(..),
      Type(..),
      ECond(..),
      Item(..),
      Stmt(..),
      Block(..),
      Param(..),
      TopDef(..),
      Program(..) )
import Data.Map ( empty, insert, lookup, Map )
import Control.Monad.Reader
    ( when,
      unless,
      foldM,
      MonadTrans(lift),
      MonadReader(local, ask),
      ReaderT(runReaderT) )
import Control.Monad.Except ( MonadError(throwError), ExceptT )
import Control.Monad.State ()
import Control.Monad ()
import Commons ( BlockDepth, Verbosity )


data VarInfo = VarInfo {dtype :: Type, depth :: BlockDepth, is_const :: Bool}
        deriving Show

type TypeEnv = Map Ident VarInfo
type TypeContext = (TypeEnv, Bool)
type TypeChecker m = ReaderT (TypeEnv, BlockDepth, Type, Bool, Verbosity)  (ExceptT String IO) m


emptyRet :: TypeChecker TypeContext
emptyRet = do
    (env, _, _, isSatisfied, _) <- ask
    return (env, isSatisfied)

debugPrint :: String -> TypeChecker ()
debugPrint s = do
    (_, _, _, _, verbosity) <- ask
    when (verbosity > 0) $ lift $ lift $ putStrLn s

expectType :: Type -> Expr -> TypeChecker Type
expectType expectedType expr = do
    actualType <- checkExpr expr
    if expectedType == actualType
        then return expectedType
        else throwError $ "Expression:" ++ show expr ++ "Expected type: " ++ show expectedType ++ " got: " ++ show actualType

verifyType :: Type -> Ident -> TypeChecker ()
verifyType expectedType ident = do
    (env, _, _, _, _) <- ask
    case Data.Map.lookup ident env of
        Just (VarInfo actualType _ _) -> do
            if actualType == expectedType
                then return ()
                else throwError $ show ident ++ ": Expected type: " ++ show expectedType ++ " got: " ++ show actualType
        Nothing -> throwError $ "Variable " ++ show ident ++ " is not declared!"

verifyArgs :: [Type] -> [Arg] -> TypeChecker ()
verifyArgs (expectedType:typeTail) ((VArg expr):argTail) = do
    expectType expectedType expr
    verifyArgs typeTail argTail
verifyArgs (expectedType:typeTail) ((PArg ident):argTail) = do
    verifyType expectedType ident
    verifyArgs typeTail argTail
verifyArgs [] [] = return ()


checkExpr :: Expr -> TypeChecker Type
checkExpr op = case op of
    EVar ident -> do
        (env, _, _, _, _) <- ask
        case Data.Map.lookup ident env of
            Just (VarInfo dtype _ _) -> return dtype
            Nothing -> throwError $ "Variable " ++ show ident ++ " is not declared!"
    ELitInt v -> return Int
    ELitTrue -> return Bool
    ELitFalse -> return Bool
    EApp ident args -> do
        (env, _, _, _, _) <- ask
        case Data.Map.lookup ident env of
            Just (VarInfo (Fun outType argTypes) _ _) -> do
                verifyArgs argTypes args
                return outType
            Just (VarInfo x _ _) -> throwError $ "Variable " ++ show ident ++ " is not a function! Actual type: " ++ show x
            Nothing -> throwError $ "Variable " ++ show ident ++ " is not declared!"
    EString _ -> return Str
    Not expr -> do
        expectType Bool expr
        return Bool
    Neg expr -> do
        expectType Int expr
        return Int
    EAdd expr1 _ expr2 -> do
        expectType Int expr1
        expectType Int expr2
        return Int
    EMul expr1 _ expr2 -> do
        expectType Int expr1
        expectType Int expr2
        return Int
    ERel expr1 _ expr2 -> do
        expectType Int expr1
        expectType Int expr2
        return Bool
    EAnd expr1 expr2 -> do
        expectType Bool expr1
        expectType Bool expr2
        return Bool
    EOr expr1 expr2 -> do
        expectType Bool expr1
        expectType Bool expr2
        return Bool

checkBlock :: Block -> TypeChecker TypeContext
checkBlock block = do
    (env, depth, expectedType, returnSatisfied, verbosity) <- ask
    (_, newReturnSatisfied) <- local (const (env, depth + 1, expectedType, returnSatisfied, verbosity)) (checkBlockStmts block)
    return (env, newReturnSatisfied)

checkBlockStmts :: Block -> TypeChecker TypeContext
checkBlockStmts (Block (h:t)) = do
    (_, depth, expectedType, _, verbosity) <- ask
    (new_env, newReturnSatisfied) <- checkStmt h
    local (const (new_env, depth, expectedType, newReturnSatisfied, verbosity)) (checkBlockStmts (Block t))

checkBlockStmts (Block []) = emptyRet

declItem :: Bool -> Type -> Item -> TypeChecker TypeContext
declItem is_const dtype (Init ident value) = do
    expectType dtype value
    declItem is_const dtype (NoInit ident)
declItem is_const dtype (NoInit ident) = do
    (env, block_depth, _, _, _) <- ask
    case Data.Map.lookup ident env of
        Just (VarInfo dtype depth is_const) -> do
            if depth == block_depth
                then throwError $ show ident ++ " is already declared in current block!"
            else
                declare
        Nothing -> declare

    where
        declare :: TypeChecker TypeContext
        declare = do
            (env, block_depth, _, returnSatisfied, _) <- ask
            let new_env = insert ident (VarInfo dtype block_depth is_const) env
            return (new_env, returnSatisfied)


declFun :: TopDef -> TypeChecker TypeContext
declFun (FnDef rettype ident params block) = do
    debugPrint $ "Analysing function: " ++ show ident
    (env, depth, expectedType, prevSat, verbosity) <- ask
    (fun_env, _) <- declParam params
    (fun_env', _) <- local (const (fun_env, depth, expectedType, prevSat, verbosity)) (declItem False (Fun rettype (map (\(Param ptype _) -> ptype) params) ) (NoInit ident))

    (_, blockSatisfies) <- local (const (fun_env', depth, rettype, False, verbosity)) (checkBlock block)
    when ((not blockSatisfies) && (rettype /= Void)) (throwError $ "Function " ++ show ident ++ " may not return value, expected to return: " ++ show rettype)
    
    return (fun_env', prevSat)
    where
        declParam :: [Param] -> TypeChecker TypeContext
        declParam ((Param ptype ident):t) = do
            (_, depth, expectedType, isSatisfied, verbosity) <- ask
            (env, _) <- declItem False ptype (NoInit ident)
            local (const (env, depth, expectedType, isSatisfied, verbosity)) (declParam t)
        declParam [] = emptyRet




declItems :: Bool -> Type -> [Item] -> TypeChecker TypeContext
declItems is_const dtype l = do
    (env, depth, expectedType, isSatisfied, verbosity) <- ask
    foldM (\(env', isSatified') item -> local (const (env', depth, expectedType, isSatified', verbosity)) (declItem is_const dtype item)) (env, isSatisfied) l

checkStmt :: Stmt -> TypeChecker TypeContext
checkStmt stmt = case stmt of
    BStmt block -> checkBlock block
    Decl dtype items -> declItems False dtype items
    FDecl fundef -> do
        (env, depth, expectedType, isSatisfied, verbosity) <- ask 
        local (const (env, depth + 1, expectedType, isSatisfied, verbosity)) (declFun fundef)
    CDecl dtype items -> declItems True dtype items
    Ass ident expr -> do
        (env, _, _, isSatified, _) <- ask
        retType <- checkExpr expr
        verifyType retType ident
        return (env, isSatified)
    Incr ident -> do
        verifyType Int ident
        emptyRet
    Decr ident -> do
        verifyType Int ident
        emptyRet
    While expr stmt -> do
        expectType Bool expr
        checkStmt stmt
        emptyRet
    For ident exprIterStart exprIterStop stmt -> do
        (env, depth, expectedType, isSatisfied, verbosity) <- ask
        (loop_env, _) <- declItem True Int (Init ident exprIterStart)
        (_, isSatified') <- local (const (loop_env, depth, expectedType, isSatisfied, verbosity)) (checkStmt stmt)
        return (env, isSatified')

    SExp expr -> do
        checkExpr expr
        emptyRet

    CondS expr block -> do
        expectType Bool expr
        checkBlock block

    CondE expr block1 block2 -> do
        (env, _, _, _, _) <- ask
        expectType Bool expr
        (_, sat1) <- checkBlock block1
        (_, sat2) <- checkBlock block2
        let sat = sat1 && sat2
        return (env, sat)

    CondC expr block econd -> do
        (env, _, _, _, _) <- ask
        expectType Bool expr
        (_, sat1) <- checkBlock block
        (_, sat2) <- checkECond econd
        let sat = sat1 && sat2
        return (env, sat)

    VRet -> do
        (env, _, expectedType, isSatisfied, _) <- ask
        case expectedType of 
            Void -> return (env, True)
            _ -> throwError "Void return in non void function found!"
    Ret expr -> do
        (env, _, expectedType, _, _) <- ask
        expectType expectedType expr
        return (env, True)
    _ -> emptyRet

checkECond :: ECond -> TypeChecker TypeContext
checkECond econd = case econd of
    ECondS expr block -> do
        expectType Bool expr
        checkBlock block
    ECondE expr block1 block2 -> do
        (env, _, _, _, _) <- ask
        expectType Bool expr
        (_, sat1) <- checkBlock block1
        (_, sat2) <- checkBlock block2
        let sat = sat1 && sat2
        return (env, sat)
    ECondC expr block econd -> do
        (env, _, _, _, _) <- ask
        expectType Bool expr
        (_, sat1) <- checkBlock block
        (_, sat2) <- checkECond econd
        let sat = sat1 && sat2
        return (env, sat)

checkProgram :: Program -> TypeChecker ()
checkProgram (Program funcdecls) = do
    debugPrint $ "Analysing program: " ++ show funcdecls
    (_, _, _, _,verbosity) <- ask
    (context, _) <- foldM (\(ctx, _) funcdecl -> local (const (ctx, 0, Void, False, verbosity)) (declFun funcdecl)) (Data.Map.empty, False) funcdecls
    case Data.Map.lookup (Ident "main") context of
        (Just (VarInfo rettype _ _)) ->
            case rettype of
                (Fun Void []) -> return ()
                _ -> throwError $ "Function main is expected to return void and dont take any arguments, get: " ++ show rettype
        Nothing -> throwError "Function main is not declared"
    debugPrint $ "Analysis complete! "
    return ()


staticCheck :: Verbosity -> Program -> ExceptT String IO ()
staticCheck verbosity p = do
    runReaderT (checkProgram p) (empty, 0, Void, False, verbosity)
    return ()

    