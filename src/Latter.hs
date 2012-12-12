module Main where

import Control.Exception (try)
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

import Parser
import Analyzer
import BackendX86

compileFile :: String -> IO ()
compileFile path = do
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
                    res <- compileX86 path
                    case res of
                        Left err -> do
                            hPutStrLn stderr $ show err
                            exitFailure
                        Right _ -> do
                            hPutStrLn stderr "OK"
                            exitSuccess

main = do
    args <- getArgs
    case args of
        [path] -> compileFile path
        otherwise -> do
            progName <- getProgName
            hPutStrLn stderr $ "Wrong arguments. Expected:\n"
                ++ progName ++ " [source file] [class name]"
            exitFailure
