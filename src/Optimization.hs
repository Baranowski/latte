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
getInstr2 :: Strategy (String, String, String)
getInstr2 = do
    s <- getLn
    assert $ (take 4 s) == "    "
    let s2 = drop 4 s
    let instr = takeWhile (/=' ') s2
    let s3 = dropWhile (==' ') (dropWhile (/=' ') s2)
    let arg0 = takeWhile (/=',') s3
    let s4 = dropWhile (==' ') $ drop 1 $ dropWhile (/=',') s3
    return (instr, arg0, s4)

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
addI i = tell ["    " ++ i]

strategies = [
      do  -- Skok do nastepnej instrukcji
        label <- isJump
        s1 <- getLn
        assert $ (take 3 s1) /= "   "
        if (s1 == "  " ++ label ++ ":")
            then addL label
            else do
                isLabel label
                tell [s1]
                addL label
    , do -- Dodawanie lub odejmowanie $0
        (i, arg0, arg1) <- getInstr2
        assert $ arg0 == "$0"
        let ipref = take 3 i
        assert $ ipref == "add" || ipref == "sub"
    , do -- "push %xxx; pop %yyy
        (i1, arg1) <- getInstr1
        (i2, arg2) <- getInstr1
        assert $ (take 4 i1) == "push"
        assert $ (take 3 i2) == "pop"
        when (arg1 /= arg2) $ addI $ "mov " ++ arg1 ++ ", " ++ arg2
    , do -- mov a, b; push b
        (i1, arg10, arg11) <- getInstr2
        assert $ (take 3 i1) == "mov"
        (i2, arg20) <- getInstr1
        assert $ (take 4 i2) == "push"
        assert $ arg11 == arg20
        addI $ i2 ++ " " ++ arg10
    , assert False ]

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
