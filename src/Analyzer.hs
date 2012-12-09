{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module Analyzer (
    rewriteProgram
) where

import qualified Data.Map as M
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import AbsCommon
import LatteAbs
import Abs2ndStage
import Builtins

data SemError = SErr Pos String
instance Show SemError where
    show (SErr (Pos l c) str) = "line " ++ (show l) ++ ", column " ++ (show c) ++ "\n" ++ str
type MyM = StateT Int (Either SemError)
data FunEnv = FunEnv {
    funs :: M.Map String (UniqId, Located LatteFun),
    classes :: M.Map String Class,
    curCl :: Maybe String}
data VarEnv = VEnv { ids :: M.Map String (UniqId, Type), names :: [String]}
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
    lookupVar :: LatteLval-> Pos -> m (LValue, Type)
    assertVarType :: LatteLval-> Type -> Pos -> m ()
instance (ErrorableMonad m) => MonadWithVars (StateT VarEnv (ReaderT FunEnv m)) where
    lookupVar lval p = do
        (first,others) <- case lval of
            [] -> semErr p ("Empty l-value")
            x:xs -> return (x,xs)
        mbeVar <- gets $ (M.lookup first) . ids
        case mbeVar of
            Nothing -> do
                inClass <- asks curCl
                when (inClass == Nothing) (semErr p ("Reference to unknown variable: " ++ first))
                lookupVar ("self":lval) p
            Just (newFirst, firstT) -> do
                (newLval, lastT) <- foldM (addSelector p) ([newFirst], firstT) others
                return (reverse newLval, lastT)
        where
          addSelector p (acc, lastT) selector = do
            clName <- case lastT of
                LtType s -> return s
                t -> semErr p ("Expected a class instance, got type: " ++ (show t))
            mbeCl <- asks $ (M.lookup clName) . classes
            cl <- case mbeCl of
                Nothing -> semErr p ("Unknown class: " ++ clName)
                Just x -> return x
            nextT <- case (M.lookup selector (fields cl)) of
                Nothing -> semErr p ("Unknown field: " ++ selector)
                Just x -> return x
            return (selector:acc, nextT)

    assertVarType lval t p = do
        (_, varT) <- lookupVar lval p
        assertTypesMatch t varT p
instance (ErrorableMonad m) => MonadWithVars (ReaderT FunVarEnv m) where
    lookupVar lval p = do
        (first,others) <- case lval of
            [] -> semErr p ("Empty l-value")
            x:xs -> return (x,xs)
        mbeVar <- asks $ (M.lookup first) . ids .vEnv
        case mbeVar of
            Nothing -> do
                inClass <- asks (curCl . fEnv)
                when (inClass == Nothing) (semErr p ("Reference to unknown variable: " ++ first))
                lookupVar ("self":lval) p
            Just (newFirst, firstT) -> do
                (newLval, lastT) <- foldM (addSelector p) ([newFirst], firstT) others
                return (reverse newLval, lastT)
        where
          addSelector p (acc, lastT) selector = do
            clName <- case lastT of
                LtType s -> return s
                t -> semErr p ("Expected a class instance, got type: " ++ (show t))
            mbeCl <- asks $ (M.lookup clName) . classes . fEnv
            cl <- case mbeCl of
                Nothing -> semErr p ("Unknown class: " ++ clName)
                Just x -> return x
            nextT <- case (M.lookup selector (fields cl)) of
                Nothing -> semErr p ("Unknown field: " ++ selector)
                Just x -> return x
            return (selector:acc, nextT)
    assertVarType lval t p = do
        (_, varT) <- lookupVar lval p
        assertTypesMatch' t varT p
instance (MonadTrans t, MonadWithVars m, Monad (t m)) => MonadWithVars (t m) where
    lookupVar lval p = lift $ lookupVar lval p
    assertVarType lval t p = lift $ assertVarType lval t p

class (Monad m) => MonadWritingVars m where
    addVariable :: String -> Type -> Pos -> m UniqId
    newEnv :: m ()
instance (ClockedMonad m, ErrorableMonad m) => MonadWritingVars (StateT VarEnv m) where
    addVariable name t p = do
        newId <- if (name == "self")
            then return name
            else nextId name
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

prettyShow [] = ""
prettyShow [x] = show x
prettyShow (x:xs) = (show x) ++ "." ++ (prettyShow xs)

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
rwtStmtDecls (Loc p (LtIncr lval)) = do
    assertVarType lval LtInt p
    (varId, _) <- lookupVar lval p
    return $ Incr varId
rwtStmtDecls (Loc p (LtDecr lval)) = do
    assertVarType lval LtInt p
    (varId, _) <- lookupVar lval p
    return $ Decr varId
rwtStmtDecls (Loc p (LtAss lval lexpr)) = do
    (varId, varT) <- lookupVar lval p
    newE <- lift $ lift $ rwtExprTyped varT lexpr
    return $ Ass varId newE
rwtStmtDecls (Loc p (LtDBlock t decls)) = do
    newDs <- forM decls rwtDecl
    return $ Blck newDs
    where
        rwtDecl (Loc dP (LtDExpr name lexpr)) = do
            newE <- lift $ lift $ rwtExprTyped t lexpr
            newId <- addVariable name t dP
            tell [Decl t newId]
            return $ Ass [newId] newE
        rwtDecl (Loc dP (LtDEmpty name)) =
            rwtDecl (Loc dP (LtDExpr name (Loc (Pos 0 0) (defaultValue t))))
rwtStmtDecls lblock@(Loc p (LtBlock stmtL)) = do
    oldEnv <- get
    newEnv
    newStmtL <- forM stmtL rwtStmtDecls
    put oldEnv
    return $ Blck newStmtL
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

defaultValue :: Type -> LatteExpr
defaultValue LtInt = LtEInt 0
defaultValue LtString = LtEStr ""
defaultValue LtBool = LtEFalse
defaultValue LtVoid = LtEVoid
defaultValue (LtType s) = LtENull s

assertTypesMatch :: (ErrorableMonad m) => Type -> Type -> Pos -> StateT VarEnv (ReaderT FunEnv m) ()
assertTypesMatch (LtType expTs) (LtType actTs) p = helper expTs actTs p expTs
    where
        helper expS actS p oldS = when (expS /= actS) $ do
            clMbe <- asks ((M.lookup actS) . classes)
            let (Just (Class super _ _)) = clMbe
            case super of
                Nothing -> semErr p ("Expected type: " ++ expS ++ ", got: " ++ oldS)
                Just s -> helper expS s p oldS
assertTypesMatch t1 t2 p =
    when (t1 /= t2) (semErr p ("Expected type: " ++ (show t1) ++ ", got: " ++ (show t2)))

assertTypesMatch' :: (ErrorableMonad m) => Type -> Type -> Pos -> ReaderT FunVarEnv m ()
assertTypesMatch' (LtType expTs) (LtType actTs) p = helper expTs actTs p expTs
    where
        helper expS actS p oldS = when (expS /= actS) $ do
            clMbe <- asks ((M.lookup actS) . classes . fEnv)
            let (Just (Class super _ _)) = clMbe
            case super of
                Nothing -> semErr p ("Expected type: " ++ expS ++ ", got: " ++ oldS)
                Just s -> helper actS s p oldS
assertTypesMatch' t1 t2 p =
    when (t1 /= t2) (semErr p ("Expected type: " ++ (show t1) ++ ", got: " ++ (show t2)))

rwtExprTyped :: Type -> (Located LatteExpr) -> StateT VarEnv (ReaderT FunEnv MyM) Expression
rwtExprTyped t lexpr@(Loc p expr) = do
    (newE, exprT) <- rwtExpr lexpr
    assertTypesMatch t exprT p
    return newE
rwtExpr :: (Located LatteExpr) -> StateT VarEnv (ReaderT FunEnv MyM) (Expression, Type)
rwtExpr lexpr = do
    varEnv <- get
    funEnv <- ask
    lift $ lift $ runReaderT (rwtExpr' lexpr) (FunVarEnv funEnv varEnv)

rwtExprTyped' :: Type -> (Located LatteExpr) -> ReaderT FunVarEnv MyM Expression
rwtExprTyped' t lexpr@(Loc p _) = do
    (newE, eT) <- rwtExpr' lexpr
    assertTypesMatch' t eT p
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
        LtType s -> rewritePnt s p newE rel lexpr2
        _ -> semErr p ("Comparison is not supported for this type: " ++ (show eT))
    where
        rewriteInt p newE rel lexpr = do
            newE2 <- rwtExprTyped' LtInt lexpr
            return (IntComp rel newE newE2, LtBool)
        rewriteOther constr t p newE rel lexpr = do
            newE2 <- rwtExprTyped' t lexpr
            when (rel `notElem` [Req, Rne]) (semErr p ("Illegal comparison operator"))
            let newR = case rel of {
                Req -> Eq ;
                _ -> Neq }
            return (constr newR newE newE2, LtBool)
        rewriteStr = rewriteOther StrComp LtString
        rewriteBool = rewriteOther BoolComp LtBool
        rewritePnt s = rewriteOther PntComp (LtType s)
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
rwtExpr' (Loc p (LtEApp [name] exprL)) = do
    funMbe <- asks ((M.lookup name) . funs . fEnv)
    case funMbe of {
      Nothing -> do
        inClass <- asks $ curCl . fEnv
        when (inClass == Nothing) (semErr p ("No such function: " ++ name))
        rwtExpr' (Loc p (LtEApp ["self",name] exprL))
    ; Just (funIdAtom, ltFun) -> do
        let funId = [funIdAtom]
        let (Loc _ (LtFun _ funT argL _)) = ltFun
        when ((length argL) /= (length exprL)) (semErr p ("Too many or too few arguments passed to function " ++ name))
        let zipL = exprL `zip` argL
        newEL <- forM zipL (\(lexpr, Loc _ (LtArg _ argT)) -> rwtExprTyped' argT lexpr)
        return (App funId newEL, funT)}
rwtExpr' (Loc p (LtEApp lval exprL)) = do
    let (mName:rest) = reverse lval
    let fields = reverse rest
    (newLval, lastT) <- lookupVar fields p
    let LtType clName = lastT
    clMbe <- asks $ (M.lookup clName) . classes . fEnv
    cl <- case clMbe of
        Nothing -> semErr p ("Unable to find unknown class: " ++ clName)
        Just x -> return x
    method <- lookupMethod mName cl
    let (Func fT args _ _) = method
    when ((length args) /= (length exprL)) (semErr p ("Too many or too few arguments passed to method" ++ mName))
    let zipL = exprL `zip` args
    newEL <- forM zipL (\(lexpr, Decl argT _) -> rwtExprTyped' argT lexpr)
    return (App newLval newEL, fT)
    where
      lookupMethod mN cl = do
        let mMbe = M.lookup mN (methods cl)
        case mMbe of
            Nothing -> do
                let (Class super _ _) = cl
                case super of
                    Nothing -> semErr p ("No such method: " ++ mN)
                    Just s -> do
                        clMbe <- asks $ (M.lookup s) . classes . fEnv
                        let (Just newCl) = clMbe
                        lookupMethod mN newCl
            Just x -> return x
rwtExpr' (Loc p LtEFalse) = return (ConstBool False, LtBool)
rwtExpr' (Loc p LtETrue) = return (ConstBool True, LtBool)
rwtExpr' (Loc p (LtEInt i)) = return (ConstInt i, LtInt)
rwtExpr' (Loc p (LtEId lval)) = do
    (newLval, t) <- lookupVar lval p
    return (EId newLval, t)
rwtExpr' (Loc p (LtENew t)) = do
    clMbe <- asks ((M.lookup t) . classes . fEnv)
    case clMbe of
        Nothing -> semErr p ("Unknown class: " ++ t)
        Just x -> return x
    return (New t, LtType t)
rwtExpr' (Loc p (LtENull t)) = do
    clMbe <- asks ((M.lookup t) . classes . fEnv)
    case clMbe of
        Nothing -> semErr p ("Unknown class: " ++ t)
        Just x -> return x
    return (Null, LtType t)

rwtFunction f@(Loc p (LtFun name retT argL lblock)) = do
    inClass <- asks curCl
    case inClass of
        Just s -> addVariable "self" (LtType s) p
        _ -> return ""
    argDecls <- forM argL $ \ (Loc p (LtArg name argT)) -> do
        argId <- addVariable name argT p
        return $ Decl argT argId
    (newBlock, localDecls) <- runWriterT $ runReaderT (rwtStmtDecls lblock) retT
    let returns = checkReturn newBlock
    when (not returns && retT /= LtVoid) (semErr p ("Function " ++ name ++ " lacks 'return' statement"))
    -- TODO: splaszczanie zagniezdzenia blokow
    return $ Func retT argDecls localDecls newBlock
    where
        checkReturn Ret = True
        checkReturn (RetExpr _) = True
        checkReturn (IfElse _ s1 s2) = (checkReturn s1) && (checkReturn s2)
        checkReturn (Blck stmtL) = checkReturn `any` stmtL
        checkReturn _ = False

getFEnv fL = do
    forM_ fL addFun
    where
        addFun lfun@(Loc p f@(LtFun name _ _ _)) = do
            env <- get
            when (name `M.member` env) (semErr p $ "Another declaration of function " ++ name)
            id <- if (name == "main")
                then return "main__"
                else nextId name
            put $ M.insert name (id, lfun) env

getClEnv clL = do
    forM_ clL addCl
    where
      addCl lcl@(Loc p cl@(LtClass name super decls funs)) = do
        env <- get
        when (name `M.member` env) (semErr p $ "Another declaration of class " ++ name)
        newDecls <- execStateT (forM_ decls getCDecl) M.empty
        methods <- execStateT (forM_ funs getMethod) M.empty
        put $ M.insert name (Class super newDecls methods) env
      getCDecl ldecl@(Loc p (LtCDecl name t)) = do
        env <- get
        when (name `M.member` env) (semErr p $ "Another declaration of field " ++ name)
        put $ M.insert name t env
      getMethod lf@(Loc p f@(LtFun name t largs _)) = do
        env <- get
        when (name `M.member` env) (semErr p $ "Another declaration of method " ++ name)
        argDecls <- forM largs $ \ (Loc _ (LtArg argN argT)) -> do
            return $ Decl argT argN
        put $ M.insert name (Func t argDecls [] (Blck [])) env

rwtClass :: LatteClass -> ReaderT FunEnv MyM Class
rwtClass (LtClass name super decls funL)= do
    newFunL <- forM funL $ \lf@(Loc p (LtFun fN _ _ _)) -> do
        newFun <- evalStateT (rwtFunction lf) (VEnv M.empty [])
        return (fN, newFun)
    clMbe <- asks $ (M.lookup name) . classes
    let Just (Class cS cFs _) = clMbe
    return $ Class cS cFs (M.fromList newFunL)

rwtProgram :: LatteTree -> MyM Program
rwtProgram (LtTop lfL lcL) = do
    fEnvBase <- execStateT (getFEnv lfL) M.empty
    let fEnv = fEnvBase `M.union` (M.fromList builtinFuncs)
    clEnv <- execStateT (getClEnv lcL) M.empty
    let fEnvBaseL = M.toList fEnvBase
    newFunL <- forM fEnvBaseL $ \(_, (id, ltFun)) -> do
        newFun <- runReaderT
            (evalStateT (rwtFunction ltFun) (VEnv M.empty []))
            (FunEnv fEnv clEnv Nothing)
        return (id, newFun)
    newCls <- forM lcL $ \lc@(Loc p cl@(LtClass name _ _ _)) -> do
        newCl <- runReaderT (rwtClass cl) (FunEnv fEnv clEnv (Just name))
        return (name, newCl)
    return $ Prog (M.fromList newFunL) (M.fromList newCls)
    where
        builtinFuncs = map fakeFunction builtins
        fakeFunction (name, Func fType args _ _) =
            (name, (name, Loc (Pos 0 0) (LtFun
                        name
                        fType
                        (map
                            (\(Decl dT dN) -> Loc (Pos 0 0) (LtArg dN dT))
                            args)
                        (Loc (Pos 0 0) (LtBlock [])))))

rewriteProgram lt = evalStateT (rwtProgram lt) 0
