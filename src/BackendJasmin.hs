module BackendJasmin(compileJasmin) where

import Control.Monad.Writer
import qualified Data.Map as M

import Abs2ndStage

data CmpError = CErr String
instance Show CmpError where
    show (CErr s) = s

addLn s = tell [s]

generateFunction :: Function -> Writer [String] ()
generateFunction func = do
    -- TODO
    return ()

generateProgram :: Program -> Writer [String] ()
generateProgram (Prog funcs) = do
    addLn ".class MainClass"
    addLn ".super java/lang/Object"
    forM_ (M.toList funcs) generateMethod
    where
        generateMethod :: (UniqId, Function) -> Writer [String] ()
        generateMethod (mId, func) = do
            addLn $ ".method static public " ++ mId
            censor (map (\s -> "    " ++ s)) (generateFunction func)
            addLn $ ".end method"

compileJasmin :: Program -> String -> IO (Either CmpError ())
compileJasmin prog execPath = do
    return $ Right ()
