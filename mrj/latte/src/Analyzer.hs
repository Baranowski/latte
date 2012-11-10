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
    deriving (Show)
type MyM = StateT Int (Either SemError)
type FunEnv = M.Map String (UniqId, Located LatteFun)
type VarEnv = M.Map String (UniqId, Type)
data FunVarEnv = FunVarEnv {fEnv :: FunEnv, vEnv :: VarEnv}

class (Monad m) => ErrorableMonad m where
    semErr :: Pos -> String -> m a
instance ErrorableMonad (Either SemError) where
    semErr p s = Left (SErr p s)
instance (MonadTrans t, ErrorableMonad m, Monad (t m)) => ErrorableMonad (t m) where
    semErr p s = lift $ semErr p s

class (Monad m) => ClockedMonad m where
    nextId :: m Int
instance ClockedMonad MyM where
    nextId = do
        n <- get
        put $ n+1
        return $ n+1
instance (MonadTrans t, ClockedMonad m, Monad (t m)) => ClockedMonad (t m) where
    nextId = lift nextId

class (Monad m) => MonadWithVars m where
    lookupVar :: String -> Pos -> m (UniqId, Type)
    assertVarType :: String -> Type -> Pos -> m ()
instance (ErrorableMonad m) => MonadWithVars (StateT VarEnv m) where
    lookupVar name p = do
        mbeVar <- gets $ M.lookup name
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
    addVariable :: String -> Type -> m UniqId
instance (ClockedMonad m) => MonadWritingVars (StateT VarEnv m) where
    addVariable name t = do
        newId <- nextId
        st <- get
        put $ M.insert name (newId, t) st
        return newId
instance (MonadTrans t, MonadWritingVars m, Monad (t m)) => MonadWritingVars (t m) where
    addVariable name tp = lift $ addVariable name tp

--lookupVar :: (MonadState s m, ErrorableMonad m) => String -> Pos -> StateT VarEnv m UniqId

-- assertVarType :: (MonadState s m, ErrorableMonad m) => String -> Type -> Pos -> StateT VarEnv m UniqId
    
-- addVariable name t -> id
-- lookupVar name pos -> (id, type)
-- assertVarType name t pos

rwtStatement :: (Located LatteStmt) ->  StateT VarEnv (ReaderT FunEnv MyM) Statement
rwtStatement (Loc _ (LtBlock stmtL)) = do
    (newStmtL, decls) <- runWriterT $ forM stmtL rwtStmtDecls
    return $ Blck decls newStmtL
rwtStatement (Loc p _) = do
    semErr p "Expecting block, got other kind of statement"
    -- this should never happen

rwtStmtDecls :: (Located LatteStmt) -> WriterT [Declaration] (StateT VarEnv (ReaderT FunEnv MyM)) Statement
rwtStmtDecls (Loc p (LtSExpr lexpr)) = do
    (newE, _) <- lift $ rwtExpr lexpr
    return $ SExpr newE
rwtStmtDecls (Loc p (LtWhile lexpr lstmt)) = do
    newE <- lift $ rwtExprTyped LtBool lexpr
    newS <- rwtStmtDecls lstmt
    return $ While newE newS
rwtStmtDecls (Loc p (LtIf lexpr lstmt1 lstmt2)) = do
    newE <- lift $ rwtExprTyped LtBool lexpr
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
    (newE, exprT) <- lift $ rwtExpr lexpr
    assertVarType name exprT p
    (varId, _) <- lookupVar name p
    return $ Ass varId newE
rwtStmtDecls (Loc p (LtDBlock t decls)) = do
    newDs <- forM decls rwtDecl
    return $ TmpFlatten newDs
    where
        rwtDecl (Loc dP (LtDExpr name lexpr)) = do
            newE <- lift $ rwtExprTyped t lexpr
            newId <- addVariable name t
            tell [Decl t newId]
            return $ Ass newId newE
        rwtDecl (Loc dP (LtDEmpty name)) = do
            newId <- addVariable name t
            tell [Decl t newId]
            return Pass
rwtStmtDecls lblock@(Loc p (LtBlock _)) = do
    newBlock <- lift $ rwtStatement lblock
    return newBlock
-- TODO: sprawdzac typ returnow
-- TODO: sprawdzac pokrycie returnami
rwtStmtDecls (Loc p (LtReturn (Loc _ LtEVoid))) = do
    return Ret
rwtStmtDecls (Loc p (LtReturn lexpr)) = do
    (newE, exprT) <- lift $ rwtExpr lexpr
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

rwtExpr' :: (Located LatteExpr) -> ReaderT FunVarEnv MyM (Expression, Type)
rwtExpr' _ = return (ConstInt 0, LtInt)

rwtFunction f@(Loc p (LtFun _ retT argL lblock)) = do
    declL <- forM argL $ \ (Loc p (LtArg name argT)) -> do
        argId <- addVariable name argT
        return $ Decl argT argId
    newBlock <- rwtStatement lblock
    return $ Func retT declL newBlock

getFEnv :: [Located LatteFun] -> StateT FunEnv MyM ()
getFEnv fL = do
    forM_ fL addFun
    where
        addFun :: (Located LatteFun) -> StateT FunEnv MyM ()
        addFun lfun@(Loc p f@(LtFun name _ _ _)) = do
            env <- get
            when (name `M.member` env) (semErr p $ "Another declaration of function " ++ name)
            id <- nextId
            put $ M.insert name (id, lfun) env

rwtProgram (LtTop lfL) = do
    fEnv <- execStateT (getFEnv lfL) M.empty
    let fEnvL = M.toList fEnv
    newFunL <- forM fEnvL $ \(_, (id, ltFun)) -> do
        newFun <- runReaderT (evalStateT (rwtFunction ltFun) M.empty) fEnv
        return (id, newFun)
    return $ Prog (M.fromList newFunL)

rewriteProgram lt = evalStateT (rwtProgram lt) 0
    

