module Builtins where

import AbsCommon
import Abs2ndStage
import qualified Data.Map as M

object = Class Nothing (M.fromList [("__vtable__", LtInt)]) M.empty

builtins :: [(String, Function)]
builtins = [
    ("printInt", Func LtVoid
        [Decl LtInt "arg1_1"]
        []
        (Blck [])),
    ("printString", Func LtVoid
        [Decl LtString "arg2_1"]
        []
        (Blck [])),
    ("error", Func LtVoid
        []
        []
        (Blck [])),
    ("readInt", Func LtInt
        []
        []
        (Blck [])),
    ("readString", Func LtString
        []
        []
        (Blck [])),
    ("strComp", Func LtBool
        [Decl LtString "arg3_1", Decl LtString "arg3_2"]
        []
        (Blck [])),
    ("strConcat", Func LtString
        [Decl LtString "arg4_1", Decl LtString "arg4_2"]
        []
        (Blck [])),
    ("modulo", Func LtInt
        [Decl LtInt "arg5_1", Decl LtInt "arg5_2"]
        []
        (Blck [])) ]
