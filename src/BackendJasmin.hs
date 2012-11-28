{-# LANGUAGE FlexibleInstances #-}
module BackendJasmin(compileJasmin) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M

import AbsCommon
import Abs2ndStage
import Builtins
import BackendJasminBuiltins

data Var = Var { reg :: Int, vT :: Type} 
data Env = Env { gT :: Type, vars :: M.Map UniqId Var, funs :: M.Map UniqId Function }

data CmpError = CErr String
instance Show CmpError where
    show (CErr s) = s

class Errorable m where
    cErr :: String -> m a
instance Errorable (Either CmpError) where
    cErr s = Left (CErr s)
instance (MonadTrans t, Errorable m, Monad m) => Errorable (t m) where
    cErr s = lift $ cErr s

type BasicMonad = WriterT [String] (Either CmpError)
type StmtMonad = StateT Int (ReaderT Env BasicMonad)

addLn s = tell [s]
-- Add instruction
addI s = addLn $ "    " ++ s
-- Add label
addL s = addLn $ "  " ++ s ++ ":"

-- Returns instruction prefix depending on the type
pI :: Type -> String
pI LtString = "a"
pI LtVoid = ""
pI LtInt = "i"
pI LtBool = "i"

newLabel :: (Monad m) => StateT Int m String
newLabel = do
    n <- get
    put (n+1)
    let sN = (show n)
    let num = ((\_->'0') `map` [(length sN)..3]) ++ sN
    return $ "Label__" ++ num

myLookup :: (Errorable m, Monad m) => UniqId-> M.Map UniqId a -> m a
myLookup id m = do
    let resMbe = M.lookup id m
    res <- case resMbe of
        Nothing -> cErr $ "Cannot find entity: " ++ id
        Just x -> return x
    return res

genStmt :: Statement -> StmtMonad ()
genStmt (Blck stmts) = forM_ stmts genStmt
genStmt Ret = (addI "ldc 0") >> (addI "ireturn")
genStmt (RetExpr e) = do
    genExpr e
    globalT <- asks gT
    addI $ (pI globalT) ++  "return"
genStmt (Ass id e) = do
    genExpr e
    vs <- asks vars
    var <- myLookup id vs
    addI $ (pI (vT var)) ++ "store_" ++ (show (reg var))
genStmt (Incr id) = do
    vs <- asks vars
    var <- myLookup id vs
    let n = reg var
    addI $ "iinc " ++ (show n) ++ " 1"
genStmt (Decr id) = do
    vs <- asks vars
    var <- myLookup id vs
    let n = reg var
    addI $ "iinc " ++ (show n) ++ " -1"
genStmt (If e s) = do
    genExpr e
    l <- newLabel
    addI $ "ifeq " ++ l
    genStmt s
    addL $ l
genStmt (IfElse e s1 s2) = do
    genExpr e
    lElse <- newLabel
    lFi <- newLabel
    addI $ "ifeq " ++ lElse
    genStmt s1
    addI $ "goto " ++ lFi
    addL $ lElse
    genStmt s2
    addL $ lFi
genStmt (While e s) = do
    lWhile <- newLabel
    lDone <- newLabel
    addL $ lWhile
    genExpr e
    addI $ "ifeq " ++ lDone
    genStmt s
    addI $ "goto " ++ lWhile
    addL $ lDone
genStmt (SExpr e) = do
    genExpr e
    addI "pop"
genStmt Pass = return ()

genExpr :: Expression -> StmtMonad ()
genExpr (Or es) = do
    lTrue <- newLabel
    lEnd <- newLabel
    forM_ es (genOrAtom lTrue)
    addI "iconst_0"
    addI $ "goto " ++ lEnd
    addL $ lTrue
    addI "iconst_1"
    addL $ lEnd
    where
        genOrAtom lTrue e = do
            genExpr e
            addI $ "ifne " ++ lTrue
genExpr (And es) = do
    lFalse <- newLabel
    lEnd <- newLabel
    forM_ es (genAndAtom lFalse)
    addI "iconst_1"
    addI $ "goto " ++ lEnd
    addL $ lFalse
    addI $ "iconst_0"
    addL $ lEnd
    where
        genAndAtom lFalse e = do
            genExpr e
            addI $ "ifeq " ++ lFalse
genExpr (IntComp rel e1 e2) = do
    lTrue <- newLabel
    lEnd <- newLabel
    genExpr e1
    genExpr e2
    addI $ "if_icmp" ++ (relToInstr rel) ++ " " ++ lTrue
    addI "iconst_0"
    addI $ "goto " ++ lEnd
    addL $ lTrue
    addI "iconst_1"
    addL $ lEnd
    where
        relToInstr Rlt = "lt"
        relToInstr Rle = "le"
        relToInstr Rgt = "gt"
        relToInstr Rge = "ge"
        relToInstr Req = "eq"
        relToInstr Rne = "ne"
genExpr (BoolComp Eq e1 e2) = genExpr (IntComp Req e1 e2)
genExpr (BoolComp Neq e1 e2) = genExpr (IntComp Rne e1 e2)
genExpr (StrComp Neq e1 e2) = genExpr (Not (StrComp Eq e1 e2))
genExpr (StrComp Eq e1 e2) =
    genExpr (App "strComp" [e1,e2])
genExpr (Arithm '%' e1 e2) = genExpr (App "modulo" [e1,e2])
genExpr (Arithm ch e1 e2) = do
    genExpr e1
    genExpr e2
    let op = case ch of
            '+' -> "iadd"
            '-' -> "isub"
            '*' -> "imul"
            '/' -> "idiv"
    addI op
genExpr (Not e) = genExpr (Arithm '-' (ConstInt 1) e)
genExpr (Neg e) = genExpr (Arithm '-' (ConstInt 0) e)
genExpr (Concat e1 e2) = genExpr (App "strConcat" [e1,e2])
genExpr (App id es) = do
    forM_ es genExpr
    fs <- asks funs
    func <- myLookup id fs
    let (Func fT args _ _) = func
    addI $ "invokestatic " ++ (funcDesc id args fT)
    where
        funcDesc id args t = "MainClass/" ++ id ++
            (funcTypeDesc args t)
genExpr (ConstBool False) = addI "iconst_0"
genExpr (ConstBool True) = addI "iconst_1"
genExpr (ConstInt n) = addI $ "ldc " ++ (show n)
genExpr (ConstStr s) = addI $ "ldc \"" ++ (esc s) ++ "\""
    where
        esc s = concat (map escCh s)
        escCh '"' = "\""
        escCh '\\' = "\\\\"
        escCh c = [c]
genExpr (EId id) = do
    vs <- asks vars
    var <- myLookup id vs
    addI $ (pI (vT var)) ++ "load_" ++ (show $ reg var)

funcTypeDesc args t = "(" ++ 
    (concat (map (\(Decl t _) -> typeDesc t) args)) ++ 
    ")" ++ (typeDesc t)
typeDesc LtString = "Ljava/lang/String;"
typeDesc LtVoid = "I"
typeDesc LtInt = "I"
typeDesc LtBool = "I"


generateFunction :: M.Map UniqId Function -> Function -> BasicMonad ()
generateFunction funcs (Func t args decls stmt) = do
    let argsN = (length args)
    let newArgs = rewriteDecl `map` (args `zip` [0..])
    let newLocals = rewriteDecl `map` (decls `zip` [argsN..])
    forM (reverse newArgs) (\(_, (Var n t)) ->
        addI $ (pI t) ++ "store_" ++ (show n))
    let vars = M.fromList (newArgs ++ newLocals)
    runReaderT (runStateT (genStmt stmt) 0) (Env t vars funcs)
    return ()
    where
        rewriteDecl ((Decl t id), n) = (id, Var n t)

generateProgram :: Program -> BasicMonad ()
generateProgram (Prog funcs) = do
    addLn ".class MainClass"
    addLn ".super java/lang/Object"
    tell builtinMethods
    forM_ (M.toList funcs) generateMethod
    tell mainMethod    
    where
        generateMethod :: (UniqId, Function) -> BasicMonad ()
        generateMethod (mId, func@(Func t args _ _)) = do
            addLn $ ".method static public " ++ mId ++ (funcTypeDesc args t)
            -- TODO
            addLn $ ".limit locals 10"
            -- TODO
            addLn $ ".limit stack 20"
            generateFunction fEnv func
            addLn $ ".end method"
        fEnv = funcs `M.union` (M.fromList builtins)

compileJasmin :: Program -> String -> IO (Either CmpError ())
compileJasmin prog execPath = do
    let res = execWriterT (generateProgram prog)
    case res of
        Left err -> return $ Left err
        Right lines -> do
            forM_ lines putStrLn
            return $ Right ()
