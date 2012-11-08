module Analyzer (
    analyze
) where

import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import AbsCommon
import LatteAbs
import Abs2ndStage

data SemError = SErr Pos String
type MyM = StateT Int (Either SemError)
type FunEnv = M.Map String (UniqId, LatteFun)
type VarEnv = M.Map String (UniqId, Type)
data FunVarEnv = FunVarEnv {fEnv :: FunEnv, vEnv :: VarEnv}

nextId :: MyM Int
nextId = do
    n <- get
    put $ n+1
    return $ n+1

addVariable :: (MonadState s m) => String -> Type -> StateT VarEnv m UniqId
addVariable name t = do
    newId <- nextId
    st <- lift get
    put $ M.insert name (newId, t) st
    return newId

lookupVar :: (MonadState s m) => String -> Pos -> StateT VarEnv m UniqId
lookupVar name p = do
    mbeVar <- lift $ gets $ M.lookup name
    case mbeVar of
        Nothing -> Left $ SErr p ("Reference to unknown variable: " ++ name)
        Just var -> return var

assertVarType :: (MonadState s m) => String -> Type -> Pos -> StateT VarEnv m UniqId
assertVarType name t p = do
    mbeVar <- lift $ gets $ M.lookup name
    case mbeVar of
        Nothing -> Left $ SErr p ("Reference to unknown variable: " ++ name)
        Just (_, varT) -> when (t /= varT) (Left $ SErr p
            ("Variable " ++ name ++ " is of type " ++ varT ++ ", expected: " ++ t))
    
-- addVariable name t -> id
-- lookupVar name pos -> (id, type)
-- assertVarType name t pos

rwtStatement :: StateT VarEnv (ReaderT FunEnv MyM) Statement
rwtStatement (Loc _ (LtBlock stmtL)) = do
    (newStmtL, decls) <- runWriterT $ forM stmtL rwtStmtDecls
    return $ Blck decls newStmtL
rwtStatement (Loc p _) = do
    Left $ SErr p "Expecting block, got other kind of statement"
    -- this should never happen

rwtStmtDecls :: WriterT [Declaration] (StateT VarEnv (ReaderT FunEnv MyM)) Statement
rwtStmtDecls (Loc p (LtSExpr lexpr)) = do
    newE <- rwtExpr lexpr
    return $ SExpr newE
rwtStmtDecls (Loc p (LtWhile lexpr lstmt)) = do
    newE <- rwtExprTyped LtBool lexpr
    newS <- rwtStmtDecls lstmt
    return $ While newE newS
rwtStmtDecls (Loc p (LtIf lexpr lstmt1 lstmt2)) = do
    newE <- rwtExprTyped LtBool lexpr
    newS <- rwtStmtDecls lstmt1
    case lstmt2 of
        Loc p LtPass -> return $ If newE newS
        _ -> do
             newS2 <- lstmt2
             return $ IfElse newE newS newS2
rwtStmtDecls (Loc p (LtIncr name)) = do
    assertVarType name LtInt
    (varId, _) <- lookupVar name p
    return $ Incr varId
rwtStmtDecls (Loc p (LtDecr name)) = do
    assertVarType name LtInt
    (varId, _) <- lookupVar name p
    return $ Decr varId
rwtStmtDecls (Loc p (LtAss name lexpr)) = do
    (newE, exprT) <- rwtExpr lexpr
    assertVarType name exprT
    (varId, _) <- lookupVar name p
    return $ Ass varId newE
rwtStmtDecls (Loc p (LtDBlock t decls)) = do
    forM decls rwtDecl
    where
        rwtDecl (Loc dP (LtDExpr name lexpr)) = do
            newE <- rwtExprTyped t lexpr
            newId <- addVariable name t
            tell (Decl t newId)
            return $ Ass newId newE
        rwtDecl (Loc dP (LtDEmpty name)) = do
            newId <- addVariable name t
            tell (Decl t newId)
            return Pass
rwtStmtDecls lblock@(Loc p (LtBlock _)) = do
    newBlock <- rwtStatement lblock
    return newBlock
-- TODO: sprawdzac typ returnow
-- TODO: sprawdzac pokrycie returnami
rwtStmtDecls (Loc p (LtReturn (Loc _ LtEVoid))) = do
    return Ret
rwtStmtDecls (Loc p (LtReturn lexpr)) = do
    (newE, exprT) <- rwtExpr lexpr
    return $ RetExpr newE
rwtStmtDecls (Loc p (LtPass)) = do
    return Pass

rwtExprTyped t lexpr@(Loc p expr) = do
    (newE, exprT) <- rwtExpr lexpr
    when (t /= exprT) (Left $ SErr p ("Expression is of type: " ++ exprT ++ ", expected: " ++ t))
    return newE
rwtExpr lexpr = do
    varEnv <- lift get
    funEnv <- ask
    runReaderT rwtExpr' (FunVarEnv funEnv varEnv) lexpr

rwtExpr' :: (Located LatteExpr) -> ReaderT FunVarEnv MyM Expression
rwtExpr' = return (LtEVoid, LtInt)

rwtFunction f@(Loc p (LtFun _ retT argL lblock)) = do
    declL <- forM argL $ \ Loc p (LtArg name argT) -> do
        argId <- addVariable name argT
        return $ Decl argT argId
    newBlock <- rwtStatement lblock
    return $ Func retT declL newBlock

getFEnv :: [Located LatteFun] -> StateT FunEnv MyM ()
getFEnv fL = do
    forM_ fL (\(Loc p f@(LtFun (LtId name) _ _ _)) -> do
        env <- lift . lift get
        when (name `M.member` env) (Left $
            Left $ SErr p "Another declaration of function " ++ name)
        id <- nextId
        lift $ put $ M.insert name (nextId, f) env)

rwtProgram (LtTop lfL) = do
    fEnv <- execStateT (getFEnv lfL) M.empty
    fL <- mapM (\ Loc f -> rwtFunction fEnv f) lfL
    return $ Prog fL

