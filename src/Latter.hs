module Main where

import Control.Exception (try)
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

import Parser
import Analyzer
import BackendJasmin

compileFile :: String -> String -> IO ()
compileFile path execPath = do
    readRes <- (try $ readFile path) :: IO (Either IOError String)
    case readRes of
        Left err -> do
            hPutStrLn stderr $ "Cannot open file: " ++ path
            exitFailure
        Right content -> case parseLatte path content of
            Left err -> do
                hPutStrLn stderr "ERROR"
                hPutStrLn stderr $ show err
                exitFailure
            Right lt -> case rewriteProgram lt of
                Left err -> do
                    hPutStrLn stderr "ERROR"
                    hPutStrLn stderr $ show err
                    exitFailure
                Right full -> do
                    --putStrLn (show full)
                    compileRes <- compileJasmin full execPath
                    case compileRes of
                        Left err -> do
                            hPutStrLn stderr "ERROR"
                            hPutStrLn stderr $ show err
                        Right _ -> do
                            hPutStrLn stderr "OK"


main = do
    args <- getArgs
    case args of
        [path, execPath] -> compileFile path execPath
        otherwise -> do
            progName <- getProgName
            hPutStrLn stderr $ "Wrong arguments. Expected:\n"
                ++ progName ++ " [source file] [class name]"
            exitFailure
