{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module Analyzer (
    rewriteProgram
) where

import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import AbsCommon
import LatteAbs
import Abs2ndStage

data SemError = SErr Pos String
instance Show SemError where
    show (SErr (Pos l c) str) = "line " ++ (show l) ++ ", column " ++ (show c) ++ "\n" ++ str
type MyM = StateT Int (Either SemError)
type FunEnv = M.Map String (UniqId, Located LatteFun)
data VarEnv = VEnv { ids :: M.Map String (UniqId, Type), names :: [String] }
data FunVarEnv = FunVarEnv {fEnv :: FunEnv, vEnv :: VarEnv}

class (Monad m) => ErrorableMonad m where
    semErr :: Pos -> String -> m a
instance ErrorableMonad (Either SemError) where
    semErr p s = Left (SErr p s)
instance (MonadTrans t, ErrorableMonad m, Monad (t m)) => ErrorableMonad (t m) where
    semErr p s = lift $ semErr p s

class (Monad m) => ClockedMonad m where
    nextId :: String -> m String
instance ClockedMonad MyM where
    nextId s = do
        n <- get
        put $ n+1
        return $ s ++ "__" ++ (show (n+1))
instance (MonadTrans t, ClockedMonad m, Monad (t m)) => ClockedMonad (t m) where
    nextId s = lift $ nextId s

class (Monad m) => MonadWithVars m where
    lookupVar :: String -> Pos -> m (UniqId, Type)
    assertVarType :: String -> Type -> Pos -> m ()
instance (ErrorableMonad m) => MonadWithVars (StateT VarEnv m) where
    lookupVar name p = do
        mbeVar <- gets $ (M.lookup name) . ids
        case mbeVar of
            Nothing -> semErr p ("Reference to unknown variable: " ++ name)
            Just var -> return var
    assertVarType name t p = do
        (_, varT) <- lookupVar name p
        when (t /= varT) (semErr p ("Variable " ++ name ++ " is of type " ++
            (show varT) ++ ", expected: " ++ (show t)))
instance (MonadTrans t, MonadWithVars m, Monad (t m)) => MonadWithVars (t m) where
    lookupVar name p = lift $ lookupVar name p
    assertVarType name t p = lift $ assertVarType name t p

class (Monad m) => MonadWritingVars m where
    addVariable :: String -> Type -> Pos -> m UniqId
    newEnv :: m ()
instance (ClockedMonad m, ErrorableMonad m) => MonadWritingVars (StateT VarEnv m) where
    addVariable name t p = do
        newId <- nextId name
        usedNames <- gets names
        when (name `elem` usedNames) (semErr p ("Variable " ++ name ++ " has been already declared"))
        st <- gets ids
        put $ VEnv {ids = (M.insert name (newId, t) st), names = (name:usedNames)}
        return newId
    newEnv = do
        oldEnv <- get
        put $ VEnv (ids oldEnv) []
instance (MonadTrans t, MonadWritingVars m, Monad (t m)) => MonadWritingVars (t m) where
    addVariable name tp p = lift $ addVariable name tp p
    newEnv = lift $ newEnv

rwtStatement :: Type -> (Located LatteStmt) ->  StateT VarEnv (ReaderT FunEnv MyM) Statement
rwtStatement t (Loc p (LtBlock stmtL)) = do
    newEnv
    (newStmtL, decls) <- runWriterT $ runReaderT (forM stmtL rwtStmtDecls) t
    return $ Blck decls newStmtL
rwtStatement t (Loc p _) = do
    semErr p "Expecting block, got other kind of statement"
    -- this should never happen

rwtStmtDecls :: (Located LatteStmt) -> ReaderT Type (WriterT [Declaration] (StateT VarEnv (ReaderT FunEnv MyM))) Statement
rwtStmtDecls (Loc p (LtSExpr lexpr)) = do
    (newE, _) <- lift $ lift $ rwtExpr lexpr
    return $ SExpr newE
rwtStmtDecls (Loc p (LtWhile lexpr lstmt)) = do
    newE <- lift $ lift $ rwtExprTyped LtBool lexpr
    newS <- rwtStmtDecls lstmt
    return $ While newE newS
rwtStmtDecls (Loc _ (LtIf (Loc _ (LtETrue)) lstmt1 _)) =
    rwtStmtDecls lstmt1;
rwtStmtDecls (Loc _ (LtIf (Loc _ (LtEFalse)) _ lstmt2)) =
    rwtStmtDecls lstmt2;
rwtStmtDecls (Loc p (LtIf lexpr lstmt1 lstmt2)) = do
    newE <- lift $ lift $ rwtExprTyped LtBool lexpr
    newS <- rwtStmtDecls lstmt1
    case lstmt2 of
        Loc p LtPass -> return $ If newE newS
        _ -> do
             newS2 <- rwtStmtDecls lstmt2
             return $ IfElse newE newS newS2
rwtStmtDecls (Loc p (LtIncr name)) = do
    assertVarType name LtInt p
    (varId, _) <- lookupVar name p
    return $ Incr varId
rwtStmtDecls (Loc p (LtDecr name)) = do
    assertVarType name LtInt p
    (varId, _) <- lookupVar name p
    return $ Decr varId
rwtStmtDecls (Loc p (LtAss name lexpr)) = do
    (newE, exprT) <- lift $ lift $ rwtExpr lexpr
    assertVarType name exprT p
    (varId, _) <- lookupVar name p
    return $ Ass varId newE
rwtStmtDecls (Loc p (LtDBlock t decls)) = do
    newDs <- forM decls rwtDecl
    return $ TmpFlatten newDs
    where
        rwtDecl (Loc dP (LtDExpr name lexpr)) = do
            newE <- lift $ lift $ rwtExprTyped t lexpr
            newId <- addVariable name t dP
            tell [Decl t newId]
            return $ Ass newId newE
        rwtDecl (Loc dP (LtDEmpty name)) = do
            newId <- addVariable name t dP
            tell [Decl t newId]
            return Pass
rwtStmtDecls lblock@(Loc p (LtBlock _)) = do
    t <- ask
    newBlock <- lift $ lift $ rwtStatement t lblock
    return newBlock
rwtStmtDecls (Loc p (LtReturn (Loc _ LtEVoid))) = do
    t <- ask
    when (t /= LtVoid) (semErr p "This function is expected to return a value")
    return Ret
rwtStmtDecls (Loc p (LtReturn lexpr)) = do
    t <- ask
    newE <- lift $ lift $ rwtExprTyped t lexpr
    return $ RetExpr newE
rwtStmtDecls (Loc p (LtPass)) = do
    return Pass

rwtExprTyped :: Type -> (Located LatteExpr) -> StateT VarEnv (ReaderT FunEnv MyM) Expression
rwtExprTyped t lexpr@(Loc p expr) = do
    (newE, exprT) <- rwtExpr lexpr
    when (t /= exprT) (semErr p ("Expression is of type: " ++ (show exprT) ++ ", expected: " ++ (show t)))
    return newE
rwtExpr :: (Located LatteExpr) -> StateT VarEnv (ReaderT FunEnv MyM) (Expression, Type)
rwtExpr lexpr = do
    varEnv <- get
    funEnv <- ask
    lift $ lift $ runReaderT (rwtExpr' lexpr) (FunVarEnv funEnv varEnv)

rwtExprTyped' :: Type -> (Located LatteExpr) -> ReaderT FunVarEnv MyM Expression
rwtExprTyped' t lexpr@(Loc p _) = do
    (newE, eT) <- rwtExpr' lexpr
    when (t /= eT) (semErr p ("Expression is of type: " ++ (show eT) ++ ", expected: " ++ (show t)))
    return newE

rwtExpr' :: (Located LatteExpr) -> ReaderT FunVarEnv MyM (Expression, Type)
rwtExpr' (Loc _ (LtEOr lexprs)) = do
    newEL <- forM lexprs (rwtExprTyped' LtBool)
    return (Or newEL, LtBool)
rwtExpr' (Loc _ (LtEAnd lexprs)) = do
    newEL <- forM lexprs (rwtExprTyped' LtBool)
    return (And newEL, LtBool)
rwtExpr' (Loc p (LtERel rel lexpr1 lexpr2)) = do
    (newE, eT) <- rwtExpr' lexpr1
    case eT of
        LtInt -> rewriteInt p newE rel lexpr2
        LtString -> rewriteStr p newE rel lexpr2
        LtBool -> rewriteBool p newE rel lexpr2
        _ -> semErr p ("Comparison is not supported for this type: " ++ (show eT))
    where
        rewriteInt p newE rel lexpr = do
            newE2 <- rwtExprTyped' LtInt lexpr
            return (IntComp rel newE newE2, LtBool)
        rewriteOther constr t p newE rel lexpr = do
            newE2 <- rwtExprTyped' t lexpr
            when (rel `notElem` [Req, Rne]) (semErr p ("Illegal operator for string comparison"))
            let newR = case rel of {
                Req -> Eq ;
                _ -> Neq }
            return (constr newR newE newE2, LtBool)
        rewriteStr = rewriteOther StrComp LtString
        rewriteBool = rewriteOther BoolComp LtBool
rwtExpr' (Loc p (LtEAdd lexpr1 exprL)) = do
    (newE1, e1T) <- rwtExpr' lexpr1
    fullE <- case e1T of
        LtInt -> foldM rewriteAdd newE1 exprL
        LtString -> foldM rewriteConcat newE1 exprL
        _ -> semErr p ("Operator is not defined for type: " ++ (show e1T))
    return (fullE, e1T)
    where
        rewriteAdd newE (op, lexpr) = do
            nextE <- rwtExprTyped' LtInt lexpr
            let opCh = case op of {
                Ladd -> '+' ;
                Lsub -> '-' }
            return $ Arithm opCh newE nextE
        rewriteConcat newE (op, lexpr) = do
            nextE <- rwtExprTyped' LtString lexpr
            when (op /= Ladd) (semErr p ("Only '+' operator is defined for type: " ++ (show LtString)))
            return $ Concat newE nextE
rwtExpr' (Loc p (LtEMul lexpr1 exprL)) = do
    newE1 <- rwtExprTyped' LtInt lexpr1
    fullE <- foldM rewriteMul newE1 exprL
    return (fullE, LtInt)
    where
        rewriteMul newE (op, lexpr) = do
            nextE <- rwtExprTyped' LtInt lexpr
            let opCh = case op of {
                Lmul -> '*' ;
                Ldiv -> '/' ;
                Lmod -> '%' }
            return $ Arithm opCh newE nextE
rwtExpr' (Loc p (LtENot lexpr)) = do
    newE <- rwtExprTyped' LtBool lexpr
    return (Not newE, LtBool)
rwtExpr' (Loc p (LtENeg lexpr)) = do
    newE <- rwtExprTyped' LtInt lexpr
    return (Neg newE, LtInt)
rwtExpr' (Loc _ (LtEStr str)) = do
    return (ConstStr str, LtString)
rwtExpr' (Loc p (LtEApp name exprL)) = do
    funMbe <- asks ((M.lookup name) . fEnv)
    (funId, ltFun) <- case funMbe of {
        Nothing -> semErr p ("No such function: " ++ name) ;
        Just res -> return res}
    let (Loc _ (LtFun _ funT argL _)) = ltFun
    when ((length argL) /= (length exprL)) (semErr p ("Too many or too few arguments passed to function " ++ name))
    let zipL = exprL `zip` argL
    newEL <- forM zipL (\(lexpr, Loc _ (LtArg _ argT)) -> rwtExprTyped' argT lexpr)
    return (App funId newEL, funT)
rwtExpr' (Loc p LtEFalse) = return (ConstBool False, LtBool)
rwtExpr' (Loc p LtETrue) = return (ConstBool True, LtBool)
rwtExpr' (Loc p (LtEInt i)) = return (ConstInt i, LtInt)
rwtExpr' (Loc p (LtEId name)) = do
    varMbe <- asks ((M.lookup name) . ids . vEnv)
    (varId, varT) <- case varMbe of {
        Nothing -> semErr p ("No such variable: " ++ name) ;
        Just res -> return res }
    return (EId varId, varT)

rwtFunction f@(Loc p (LtFun name retT argL lblock)) = do
    declL <- forM argL $ \ (Loc p (LtArg name argT)) -> do
        argId <- addVariable name argT p
        return $ Decl argT argId
    newBlock <- rwtStatement retT lblock
    let returns = checkReturn newBlock
    when (not returns && retT /= LtVoid) (semErr p ("Function " ++ name ++ " lacks 'return' statement"))
    return $ Func retT declL newBlock
    where
        checkReturn Ret = True
        checkReturn (RetExpr _) = True
        checkReturn (Blck _ stmtL) = checkReturn `any` stmtL
        checkReturn (IfElse _ s1 s2) = (checkReturn s1) && (checkReturn s2)
        checkReturn (TmpFlatten stmtL) = checkReturn `any` stmtL
        checkReturn _ = False

getFEnv :: [Located LatteFun] -> StateT FunEnv MyM ()
getFEnv fL = do
    forM_ fL addFun
    where
        addFun :: (Located LatteFun) -> StateT FunEnv MyM ()
        addFun lfun@(Loc p f@(LtFun name _ _ _)) = do
            env <- get
            when (name `M.member` env) (semErr p $ "Another declaration of function " ++ name)
            id <- nextId name
            put $ M.insert name (id, lfun) env

rwtProgram (LtTop lfL) = do
    fEnv <- execStateT (getFEnv lfL) M.empty
    let fEnvL = M.toList fEnv
    newFunL <- forM fEnvL $ \(_, (id, ltFun)) -> do
        newFun <- runReaderT (evalStateT (rwtFunction ltFun) (VEnv M.empty [])) (fEnv `M.union` (M.fromList functions))
        return (id, newFun)
    return $ Prog (M.fromList newFunL)
    where
        -- TODO: napisac to ladniej
        fakeFunction fakeId name fType argTypes =
            (name, ((name ++ "__" ++ fakeId) , Loc (Pos 0 0) (LtFun name fType
                ((\t -> Loc (Pos 0 0) (LtArg "" t)) `map` argTypes) (Loc (Pos 0 0) (LtBlock [])))))
        functions = [
            (fakeFunction "m1" "printInt" LtInt [LtInt]),
            (fakeFunction "m2" "printString" LtString [LtString]),
            (fakeFunction "m3" "error" LtVoid []),
            (fakeFunction "m4" "readInt" LtInt []),
            (fakeFunction "m5" "readString" LtString []) ]

rewriteProgram lt = evalStateT (rwtProgram lt) 0
    

