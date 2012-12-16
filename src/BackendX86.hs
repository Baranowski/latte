{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module BackendX86(compileX86, CmpError(..)) where

import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import AbsCommon
import Abs2ndStage

data CmpError = CErr String
instance Show CmpError where
    show (CErr s) = s

class Errorable m where
    cErr :: String -> m a
instance Errorable (Either CmpError) where
    cErr s = Left $ CErr s
instance (MonadTrans t, Errorable m, Monad m) => Errorable (t m) where
    cErr s = lift $ cErr s

data AsmProg = AsmProg {
    constants :: [(String, String) ],
    functions :: [(String, [String])]
    }
toAsm (AsmProg consts funs) =
    concat $ (map constToAsm consts) ++ (map funToAsm funs)
--TODO escaping
constToAsm (name, const) = name ++ ":\n    .ascii \"" ++ const ++ "\0\"\n"
funToAsm (name, body) = name ++ ":\n" ++ (concat (map (++"\n") body))
instance Show AsmProg where
    show a = toAsm a

instance Monoid AsmProg where
    mempty = AsmProg [] []
    mappend (AsmProg cs1 fs1) (AsmProg cs2 fs2) =
        (AsmProg (cs1++cs2) (fs1++fs2))

addFunction :: String -> [String] -> MainWriter ()
addFunction name body = tell $ mempty { functions = [(name, body)] }
addConstants consts = tell $ mempty { constants = consts }

data ClassInfo = ClassInfo {
    ciFields :: M.Map String (Int, Type),
    ciMethods :: M.Map String Int,
    ciFieldL :: [Type]
    }

type BaseMonad = StateT Int (Either CmpError)
type MainWriter = WriterT AsmProg (ReaderT (M.Map String Class) BaseMonad)

data StmtEnv = StmtEnv {
    seClasses :: M.Map String ClassInfo,
    seOffsets :: M.Map String Int,
    seTypes :: M.Map String Type,
    seEndL :: String
    }

type LocalWriter = WriterT ([String], [(String, String)]) BaseMonad
type LocalRWriter = ReaderT StmtEnv LocalWriter

newCounter :: (MonadState Int m) => m String
newCounter = do
    old <- get
    let oldS = show old
    let res = (map (\_ -> '0') [(length (show oldS))..4]) ++ oldS
    put $ old + 1
    return res

newLabel :: (MonadState Int m) => m String
newLabel = do
    cnt <- newCounter
    return $ "L" ++ cnt
newConst = do
    cnt <- newCounter
    return $ "Const" ++ cnt

addI s = tell (["    " ++ s], [])
addL s = tell (["  " ++ s ++ ":"], [])
addC n s = tell ([], [(n, constEscape s)])

--TODO
constEscape s = s

-- Only when we know for sure that the key exists in map
myLookup k mp = do
    let resMbe = M.lookup k mp
    case resMbe of
        Just x -> return x
        Nothing -> cErr $ "Could not find: " ++ (show k) ++ " (this should not have happened)"

clName :: Type -> String
clName (LtType s) = s
clName _ = "_nonexistent_"

-- Oblicz adres zmiennej i wrzuc do EAX
-- Po drodze uzywa (zasmieca) EBX
computeAddr :: LValue -> LocalRWriter ()
computeAddr lval = do
    let (localV:tl) = lval
    offM <- asks seOffsets
    off <- myLookup localV offM
    tpM <- asks seTypes
    tp <- myLookup localV tpM
    (inR, outR) <- registers lval
    addI $ "leal $" ++ (show off) ++ "(%ebp), " ++ outR
    resolveAddr (clName tp) tl
    where
        resolveAddr _ [] = return ()
        resolveAddr s lval@(first:tl) = do
            clM <- asks seClasses
            clInfo <- myLookup s clM
            (fieldOff, tp) <- myLookup first (ciFields clInfo)
            (inR, outR) <- registers lval
            addI $ "movl $" ++ (show fieldOff) ++ "(" ++ inR
                ++ "), " ++ outR
            resolveAddr (clName tp) tl
        registers l = do
            let len = (length l)
            case (len `mod` 2) of
                1 -> return ("%ebx", "%eax")
                _ -> return ("%eax", "%ebx")

rwtCondNot notL (And es) =
    forM_ es (rwtCondNot notL)
rwtCondNot notL (Or es) = do
    orEndL <- newLabel
    forM_ es (rwtCond orEndL)
    addI $ "jmp   " ++ notL
    addL orEndL
rwtCondNot notL (Not e) = rwtCond notL e
rwtCondNot notL (ConstBool False) =
    addI $ "jmp   " ++ notL
rwtCondNot notL (ConstBool True) = return ()
rwtCondNot notL (StrComp Eq e1 e2) =
    rwtCondNot notL (Not (App ["strComp"] [e1, e2]))
rwtCondNot notL (StrComp Neq e1 e2) =
    rwtCond notL (App ["strComp"] [e1, e2])
rwtCondNot notL (PntComp rel e1 e2) =
    rwtCondNot notL (IntComp (eqToIRelation rel) e1 e2)
rwtCondNot notL (BoolComp rel e1 e2) =
    rwtCondNot notL (IntComp (eqToIRelation rel) e1 e2)
rwtCondNot notL (IntComp rel e1 e2) = do
    rwtExpr e1
    addI $ "push  %eax"
    rwtExpr e2
    addI $ "pop   %ebx"
    addI $ "cmp   %eax, %ebx" 
    let instr = case rel of
                    Rlt -> "jge"
                    Rle -> "jg "
                    Rgt -> "jle"
                    Rge -> "jl "
                    Req -> "jne"
                    Rne -> "je "
    addI $ instr ++ "   " ++ notL
rwtCondNot notL e@(App _ _) = do
    rwtExpr e
    addI $ "cmp   $0,   %eax" 
    addI $ "je    " ++ notL
rwtCondNot notL e@(EId _) = do
    rwtExpr e
    addI $ "cmp   $0,   %eax" 
    addI $ "je    " ++ notL
rwtCondNot notL e = do
    cErr $ "Unsupported condition type: " ++ (show e)

rwtCond label (Or es) =
    forM_ es (rwtCond label)
rwtCond label (And es) = do
    notL <- newLabel
    forM_ es (rwtCond notL)
    addI $ "jmp   " ++ label
    addL notL
rwtCond label (Not e) =
    rwtCondNot label e
rwtCond label (ConstBool False) = return ()
rwtCond label (ConstBool True) =
    addI $ "jmp   " ++ label
rwtCond label (StrComp Eq e1 e2) =
    rwtCond label (App ["strComp"] [e1, e2])
rwtCond label (StrComp Neq e1 e2) =
    rwtCond label (Not (App ["strComp"] [e1, e2]))
rwtCond label (PntComp rel e1 e2) =
    rwtCond label (IntComp (eqToIRelation rel) e1 e2)
rwtCond label (BoolComp rel e1 e2) =
    rwtCond label (IntComp (eqToIRelation rel) e1 e2)
rwtCond label (IntComp rel e1 e2) = do
    rwtExpr e1
    addI $ "push  %eax"
    rwtExpr e2
    addI $ "pop   %ebx"
    addI $ "cmp   %eax, %ebx" 
    let instr = case rel of
                    Rlt -> "jl "
                    Rle -> "jle"
                    Rgt -> "jg "
                    Rge -> "jge"
                    Req -> "je "
                    Rne -> "jne"
    addI $ instr ++ "   " ++ label
rwtCond label e@(App _ _) = do
    rwtExpr e
    addI $ "cmp   $0,   %eax" 
    addI $ "jne   " ++ label
rwtCond label e@(EId _) = do
    rwtExpr e
    addI $ "cmp   $0,   %eax" 
    addI $ "jne   " ++ label
rwtCond _ e = do
    cErr $ "Unsupported condition type: " ++ (show e)

eqToIRelation Eq = Req
eqToIRelation Neq = Rne

rwtExpr :: Expression -> LocalRWriter ()
rwtExpr (EId lval) = do
    --TODO
    return ()
rwtExpr (App lval es) = do
    --TODO
    return ()
rwtExpr (New s) = do
    --TODO
    return ()
rwtExpr (Arithm op e1 e2) = do
    rwtExpr e1
    addI $ "pushl  %eax"
    rwtExpr e2
    addI $ "popl  %ebx"
    addI $ "xchg  %eax, %ebx"
    case op of
      '+' -> addI $ "add   %ebx, %eax"
      '-' -> addI $ "sub   %ebx, %eax"
      '*' -> addI $ "imul  %ebx"
      '/' -> do
                addI $ "xor   %edx, %edx"
                addI $ "idiv  %ebx"
      '%' -> do
                addI $ "xor   %edx, %edx"
                addI $ "idiv  %ebx"
                addI $ "mov   %edx, %eax"
      _ -> cErr $ "Unsupported arithmetic operation: " ++ [op]
rwtExpr (Neg e) = do
    rwtExpr e
    addI $ "neg   %eax"
rwtExpr (Concat e1 e2) =
    rwtExpr (App ["strConcat"] [e1,e2])
rwtExpr (ConstInt i) =
    addI $ "mov  $" ++ (show i) ++ ", %eax"
rwtExpr (Null) =
    rwtExpr (ConstInt 0)
rwtExpr (ConstStr s) = do
    cN <- newConst
    addC cN s
    addI $ "mov $" ++ cN ++ ", %eax"
-- Logical expressions - treat them like statement:
-- if not expr then %eax := 0 else %eax = 1
rwtExpr e = do
    trueL <- newLabel
    finalL <- newLabel
    rwtCond trueL e
    addI "mov   $0,  %eax"
    addI $ "jmp   " ++ finalL
    addL trueL
    addI $ "mov   $1,  %eax"
    addL finalL

rwtStmt :: Statement -> LocalRWriter ()
rwtStmt (Blck stmts) = forM_ stmts rwtStmt
rwtStmt Ret = do
    endLabel <- asks seEndL
    addI $ "jmp   " ++ endLabel
rwtStmt (Incr lval) = do
    computeAddr lval
    addI $ "inc   (%eax)"
rwtStmt (Decr lval) = do
    computeAddr lval
    addI $ "dec   (%eax)"
rwtStmt (RetExpr e) = do
    rwtExpr e
    endLabel <- asks seEndL
    addI $ "jmp   " ++ endLabel
rwtStmt (Ass lval e) = do
    rwtExpr e
    addI $ "mov   %eax, %ecx"
    computeAddr lval
    addI $ "mov   %ecx, (%eax)"
rwtStmt (IfElse e sIf sElse) = do
    elseL <- newLabel
    fiL <- newLabel
    rwtCondNot elseL e
    rwtStmt sIf
    addI $ "jmp   " ++ fiL
    addL elseL
    rwtStmt sElse
    addL fiL
rwtStmt (If e s) = do
    fiL <- newLabel
    rwtCondNot fiL e
    rwtStmt s
    addL fiL
rwtStmt (While e s) = do
    whileL <- newLabel
    whileEndL <- newLabel
    addL whileL
    rwtCondNot whileEndL e
    rwtStmt s
    addI $ "jmp   " ++ whileL
    addL whileEndL
rwtStmt (SExpr e) = do
    rwtExpr e
rwtStmt SEmpty = return ()
rwtStmt Pass = return ()

rwtFunBody :: (M.Map String Class) -> Function -> LocalWriter ()
rwtFunBody clM fun@(Func t args decls stmt) = do 
    let argsZ = (reverse args) `zip` (map (*4) [1..])
    let declsZ = decls `zip` (map (*(0-4)) [1..])
    let varsZ = argsZ ++ declsZ
    let offM = M.fromList $ map (\(Decl vT vId, off) -> (vId, off)) varsZ
    let tM = M.fromList $ map (\(Decl vT vId, _ ) -> (vId, vT)) varsZ
    endLabel <- newLabel
    let sEnv = StmtEnv clM offM tM endLabel
    addI "pushl  %ebp"
    addI "movl   %esp,  %ebp"
    addI $ "subl   $" ++ (show $ 4 * (length decls)) ++ ",  %esp"
    runReaderT (rwtStmt stmt) sEnv
    addL endLabel
    addI "leave"
    addI "ret"

rwtFunction :: String -> Function -> MainWriter ()
rwtFunction name fun@(Func t args decls stmt) = do
    clsM <- ask
    (body, constants) <- lift $ lift $ execWriterT (rwtFunBody clsM fun)
    addFunction name body
    addConstants constants

rewriteProgram :: Program -> MainWriter ()
rewriteProgram prog@(Prog funM clM) = do
    forM_ (M.toList funM) (\(name, fun) -> rwtFunction name fun)

compileX86 :: Program -> String -> IO (Either CmpError ())
compileX86 prog@(Prog funM clM) path = do
    let res = evalStateT (runReaderT (execWriterT (rewriteProgram prog)) clM) 0
    case res of
        Left cErr -> return $ Left cErr
        Right asmProg -> do
            putStrLn $ show asmProg
            return $ Right ()
