module BackendJasmin(compileJasmin) where

import Abs2ndStage
import Data.Map as M

data CmpError = CErr String
instance Show CmpError where
    show (CErr s) = s

addLn s = tell [s]

generateMethod :: Function -> Writer [String] ()
generateMethod func = do
    -- TODO
    return ()

generateProgram :: Program -> Writer [String] ()
generateProgram (Prog funcs) = do
    addLn ".class MainClass"
    addLn ".super java/lang/Object"
    forM_ (M.toList funcs) generateMethod
    where
        generateMethod (mId, func) = do
            addLn $ ".method static public" ++ mid
            censor (map (\s -> "    " ++ s)) (generateMethod func)
            addLn $ ".end method"

compileJasmin :: Program -> String -> IO (Either CmpError ())
compileJasmin prog execPath = do
    return $ Right ()
