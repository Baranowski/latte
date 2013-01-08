{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module BackendX86(compileX86, CmpError(..)) where

import qualified Data.Map as M
import System.IO
import System.Process(system)
import Data.Monoid
import Data.List.Utils as LU
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
    concat $ [".globl main__\n"] ++ (map constToAsm consts) ++ (map funToAsm funs)
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
empyClassInfo = ClassInfo M.empty M.empty []

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

defaultValue :: Type -> Expression
defaultValue LtBool = ConstBool False
defaultValue LtInt = ConstInt 0
defaultValue LtString = ConstStr ""
defaultValue (LtType s) = Null

-- Oblicz adres zmiennej i wrzuc do EAX
-- Po drodze uzywa (zasmieca) EBX
-- Dziala tylko dla niepustego lval
computeAddr :: LValue -> LocalRWriter String
computeAddr lval = do
    let (localV:tl) = lval
    offM <- asks seOffsets
    off <- myLookup localV offM
    tpM <- asks seTypes
    tp <- myLookup localV tpM
    (inR, outR) <- registers lval
    addI $ "leal " ++ (show off) ++ "(%ebp), " ++ outR
    resolveAddr (clName tp) tl
    where
        resolveAddr t [] = return t
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
    rwtCondNot notL (Not (App ["strComp"] [e2, e1]))
rwtCondNot notL (StrComp Neq e1 e2) =
    rwtCond notL (App ["strComp"] [e2, e1])
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
    forM_ es (rwtCondNot notL)
    addI $ "jmp   " ++ label
    addL notL
rwtCond label (Not e) =
    rwtCondNot label e
rwtCond label (ConstBool False) = return ()
rwtCond label (ConstBool True) =
    addI $ "jmp   " ++ label
rwtCond label (StrComp Eq e1 e2) =
    rwtCond label (App ["strComp"] [e2, e1])
rwtCond label (StrComp Neq e1 e2) =
    rwtCond label (Not (App ["strComp"] [e2, e1]))
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
    computeAddr lval
    addI $ "mov   %eax, %ebx"
    addI $ "mov   (%ebx), %eax"
rwtExpr (App [fName] es) = do
    forM_ es addParam
    addI $ "call  " ++ fName
    addI $ "add   $" ++ (show $ 4 * (length es)) ++ ", %esp"
    where
      addParam e = do
        rwtExpr e
        addI $ "push  %eax"
rwtExpr (App lval es) = do
    let objLval = reverse $ tail $ reverse lval
    let methodName = head $ reverse lval
    clN <- computeAddr objLval
    -- Wrzuc "self" na stos
    addI $ "push  %eax"
    -- Wrzuc pozostale argumenty
    forM_ es addParam
    -- Znajdz adres obiektu
    addI $ "mov   (%esp,$-" ++ (show $ length es) ++ ",4), %ebx"
    -- Adres v-table
    addI $ "mov   (%ebx), %ecx"
    -- Adres metody
    classes <- asks seClasses
    clInfo <- myLookup clN classes
    methodNum <- myLookup methodName (ciMethods clInfo)
    addI $ "mov   (%ecx," ++ (show methodNum) ++ ",4), %eax"
    addI $ "call  (%eax)"
    addI $ "add   " ++ (show $ 4 * ((length es) + 1)) ++ ", %esp"
    where
      addParam e = do
        rwtExpr e
        addI $ "push  %eax"
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
    rwtExpr (App ["strConcat"] [e2,e1])
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
    addI $ "incl   (%eax)"
rwtStmt (Decr lval) = do
    computeAddr lval
    addI $ "decl   (%eax)"
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

genClassInfos :: M.Map String Class -> StateT (M.Map String ClassInfo) (Either CmpError) ()
genClassInfos clM = do
  forM_ (M.toList clM) tryAddClass
  acc <- get
  when ((M.size acc) < (M.size clM)) (genClassInfos clM)
  where
    tryAddClass (s, cl) =
        case super cl of
            Nothing -> addExtendedClass empyClassInfo s cl
            Just s -> do
                acc <- get
                case (M.lookup s acc) of
                    Nothing -> return ()
                    Just base -> addExtendedClass base s cl
    addExtendedClass baseCI name cl = do
        let oldFieldN = M.size (ciFields baseCI)
        -- [(num, (name, type))]
        let fieldsL = [oldFieldN..] `zip` (M.toList $ fields cl)
        let newFieldL = (ciFieldL baseCI) ++ (map (snd . snd) fieldsL)
        let newFields = M.union (ciFields baseCI) $ \
            M.fromList (map (\(id, (name, t)) -> (name, (id, t))) fieldsL) 
        let oldMethodN = M.size (ciMethods baseCI) 
        -- [(num, (name, fun))]
        let methodsL = [oldMethodN..] `zip` (M.toList $ methods cl)
        let newMethods = M.union (ciMethods baseCI) $ \
            M.fromList (map (\(id, (name, _)) -> (name, id)) methodsL)
        modify $ M.insert name (ClassInfo newFields newMethods newFieldL)


rwtFunBody :: (M.Map String Class) -> Function -> LocalWriter ()
rwtFunBody clM fun@(Func t args decls stmt) = do 
    let argsZ = (reverse args) `zip` (map (*4) [2..])
    let declsZ = decls `zip` (map (*(0-4)) [1..])
    let varsZ = argsZ ++ declsZ
    let offM = M.fromList $ map (\(Decl vT vId, off) -> (vId, off)) varsZ
    let tM = M.fromList $ map (\(Decl vT vId, _ ) -> (vId, vT)) varsZ
    endLabel <- newLabel
    clIM <- lift $ lift $ execStateT (genClassInfos clM) M.empty
    let sEnv = StmtEnv clIM offM tM endLabel
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
            let (sourceDir, sourceFile) = splitPath path
            let asmPath = sourceDir ++ "/" ++ (filenameLatToAsm sourceFile)
            writeCode asmPath asmProg
            invokeAsm sourceDir asmPath
    where
        splitPath path =
            let (file:dirL) = reverse (split "/" path)
                in ((LU.join "/" (reverse dirL)), file)
        filenameLatToAsm fileN = head (split "." fileN) ++ ".s"

invokeAsm dir path = do
    system $ "gcc -o " ++ dir ++ "/a.out " ++ RUNTIME_PATH ++ " " ++ path
    return $ Right ()

writeCode path prog = do
    h <- openFile path WriteMode
    hPutStr h (toAsm prog)
    hClose h
