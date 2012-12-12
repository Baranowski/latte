module BackendX86(compileX86, CmpError(..)) where

import qualified Data.Map as M
import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State

import Abs2ndStage

data CmpError = CErr String
instance Show CmpError where
    show (CErr s) = s

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


type BaseMonad = StateT Int (Either CmpError)
type MainWriter = WriterT AsmProg (ReaderT (M.Map String Class) BaseMonad)

data StmtEnv = StmtEnv {
    classes :: M.Map String Class,
    offsets :: M.Map String Int,
    types :: M.Map String Type,
    endL :: String
    }

type LocalWriter = WriterT [String] BaseMonad
type LocalRWriter = ReaderT StmtEnv LocalWriter

newLabel = do
    old <- get
    let oldS = show old
    let res = "L" ++ (map (\_ -> '0') [(length (show oldS))..4]) ++ oldS
    put $ old + 1
    return res

addI s = tell ["    " ++ s]
addL s = tell ["  " ++ s ++ ":"]

rwtStmt :: Statement -> LocalRWriter ()
rwtStmt (Blck stmts) = forM_ stmts rwtStmt
rwtStmt Ret = do
    endLabel <- asks endL
    addI $ "jmp   " ++ endLabel
-- TODO
rwtStmt _ = rwtStmt Ret
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
    body <- lift $ lift $ execWriterT (rwtFunBody clsM fun)
    addFunction name body

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
