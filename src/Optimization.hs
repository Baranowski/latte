module Optimization(optimize) where

import Control.Monad.State
import Control.Monad.Writer

type Strategy = StateT [String] (WriterT [String] Maybe)

assert :: Bool -> Strategy ()
assert b = do
    when (not b) $ lift $ lift Nothing

getLn :: Strategy String
getLn = do
    ss <- get
    assert $ not $ null ss
    put $ tail ss
    return $ head ss
getInstr1 :: Strategy (String, String)
getInstr1 = do
    s <- getLn
    assert $ (take 4 s) == "    "
    let s2 = drop 4 s
    let instr = takeWhile (/=' ') s2
    let s3 = dropWhile (==' ') (dropWhile (/=' ') s2)
    return (instr, s3)

isJump :: Strategy String
isJump = do
    (s, dest) <- getInstr1
    assert $ (take 1 s) == "j"
    return dest
isLabel :: String -> Strategy ()
isLabel l = do
    s <- getLn
    assert $ s == ("  " ++ l ++ ":")
    
addL :: String -> Strategy ()
addL l = tell ["  " ++ l ++ ":"]

strategies = [
    do  -- Skok do nastepnej instrukcji
        label <- isJump
        isLabel label
        addL label]

scroll :: [String] -> [String] -> Strategy () -> [String]
scroll [] acc _ = reverse acc
scroll (ssh:sst) acc stg =
    let res = runWriterT (execStateT stg (ssh:sst)) in
        case res of
            Nothing -> scroll sst (ssh:acc) stg
            Just (newSs, newCodeR) -> scroll newSs ((reverse newCodeR) ++ acc) stg

applyStrategy ss stg = scroll ss [] stg

optimize :: [String] -> [String]
optimize ss = foldl applyStrategy ss strategies
