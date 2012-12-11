{-# LANGUAGE FlexibleInstances #-}
module BackendJasmin(compileJasmin) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.List.Utils as L
import System.IO.Temp(openTempFile)
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO(hPutStrLn, stdout, hClose, openFile, IOMode(..))
import System.Cmd(system)
import System.Exit(ExitCode(..))

import AbsCommon
import Abs2ndStage
import Builtins
import BackendJasminBuiltins

data Var = Var { reg :: Int, vT :: Type} 
data Env = Env { gT :: Type, vars :: M.Map UniqId Var, funs :: M.Map UniqId Function, classN :: String }

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
    className <- asks classN
    addI $ "invokestatic " ++ className ++ "/" ++ (funcDesc id args fT)
    where
        funcDesc id args t = id ++ (funcTypeDesc args t)
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


generateFunction :: M.Map UniqId Function -> Function -> String -> BasicMonad ()
generateFunction funcs (Func t args decls stmt) className = do
    let argsN = (length args)
    let newArgs = rewriteDecl `map` (args `zip` [0..])
    let newLocals = rewriteDecl `map` (decls `zip` [argsN..])
    let vars = M.fromList (newArgs ++ newLocals)
    runReaderT (runStateT (genStmt stmt) 0) (Env t vars funcs className)
    runReaderT (runStateT (genStmt Ret) 0) (Env t vars funcs className)
    return ()
    where
        rewriteDecl ((Decl t id), n) = (id, Var n t)

generateProgram :: Program -> String -> BasicMonad ()
generateProgram (Prog funcs) className= do
    addLn $ ".class " ++ className
    addLn ".super java/lang/Object"
    tell $ map (replace classNameMacro className) builtinMethods
    forM_ (M.toList funcs) generateMethod
    tell $ map (replace classNameMacro className) mainMethod
    where
        generateMethod :: (UniqId, Function) -> BasicMonad ()
        generateMethod (mId, func@(Func t args decls stmt)) = do
            addLn $ ".method static public " ++ mId ++ (funcTypeDesc args t)
            addLn $ ".limit locals " ++ ( show $ (length args) + (length decls))
            -- TODO
            addLn $ ".limit stack " ++ (show $ limitStackStmt stmt)
            generateFunction fEnv func className
            addLn $ ".end method"
        fEnv = funcs `M.union` (M.fromList builtins)

limitStackStmt (Blck stmts) = foldl max 1 (map limitStackStmt stmts)
limitStackStmt (RetExpr e) = limitStackExpr e
limitStackStmt (Ret) = 1
limitStackStmt (Ass _ e) = limitStackExpr e
limitStackStmt (If e s) = max (limitStackStmt s) $ 1 + (limitStackExpr e)
limitStackStmt (IfElse e s1 s2) = max (1 + (limitStackStmt s1)) $ max (1 + (limitStackStmt s2)) (limitStackExpr e)
limitStackStmt (While e s) = max (limitStackStmt s) (limitStackExpr e)
limitStackStmt (SExpr e) = limitStackExpr e
limitStackStmt _ = 0

limitStackExpr (Or es) = foldl max 1 (map limitStackExpr es)
limitStackExpr (And es) = foldl max 1 (map limitStackExpr es)
limitStackExpr (IntComp _ e1 e2) = max (limitStackExpr e1) (1 + (limitStackExpr e2))
limitStackExpr (BoolComp _ e1 e2) = max (limitStackExpr e1) (1 + limitStackExpr e2)
limitStackExpr (StrComp _ e1 e2) = limitStackExpr (App "" [e1,e2])
limitStackExpr (Arithm '%' e1 e2) = limitStackExpr (App "" [e1,e2])
limitStackExpr (Arithm _ e1 e2) = max (limitStackExpr e1) (1 + (limitStackExpr e2))
limitStackExpr (Neg e) = limitStackExpr (Arithm '-' (ConstInt 0) e)
limitStackExpr (Not e) = limitStackExpr (Arithm '-' (ConstInt 1) e)
limitStackExpr (Concat e1 e2) = max (limitStackExpr e1) (1+(limitStackExpr e2))
limitStackExpr (App _ es) =
    let numbered = es `zip` [0..] in
        foldl max 1 (map (\(e,n) -> n + (limitStackExpr e)) numbered)
limitStackExpr _ = 1

invokeJasmin :: [String] -> String -> String -> IO (Either CmpError ())
invokeJasmin lines path className = do
    let jasminPath = path ++ "/" ++ className ++ ".j"
    fileH <- openFile jasminPath WriteMode
    forM_ lines (hPutStrLn fileH)
    hClose fileH
    exitCode <- system $ "java -jar " ++ JASMIN_DIR ++ "/jasmin.jar -d " ++ path ++ " " ++ jasminPath ++ " > /dev/null 2> /dev/null"
    let res = case exitCode of
            ExitFailure i -> Left $ CErr $ "jasmin exited with code: " ++ (show i)
            _ -> Right ()
    return res

compileJasmin :: Program -> String -> IO (Either CmpError ())
compileJasmin prog path = do
    let pathL = split "/" path
    let pathDir = L.join "/" $ reverse $ tail $ reverse pathL
    let className = head $ split "." $ head $ reverse pathL
    let res = execWriterT (generateProgram prog className)
    case res of
        Left err -> return $ Left err
        Right lines -> invokeJasmin lines pathDir className
