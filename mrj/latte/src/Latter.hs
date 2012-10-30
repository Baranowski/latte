module Main where

import Control.Exception (try)
import System.Environment (getProgName, getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import Parser

compileFile :: String -> IO ()
compileFile path = do
    readRes <- (try $ readFile path) :: IO (Either IOError String)
    case readRes of
        Left err -> do
            hPutStrLn stderr $ "Nie mozna otworzyc pliku: " ++ path
            exitFailure
        Right content -> print $ show $ parse content

main = do
    args <- getArgs
    case args of
        [path] -> compileFile path
        otherwise -> do
            progName <- getProgName
            hPutStrLn stderr $ "Nieprawidlowe wywolanie programu. Oczekiwano:\n"
                ++ progName ++ " [plik zrodlowy]"
            exitFailure

            
