module Main where
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)

compileFile :: String -> IO ()
compileFile _ = return ()

main = do
    args <- getArgs
    case args of
        [path] -> compileFile path
        otherwise -> do
            progName <- getProgName
            hPutStrLn stderr $ "Nieprawidlowe wywolanie programu. Oczekiwano:\n"
                ++ progName ++ " [plik zrodlowy]"
            exitFailure

            
