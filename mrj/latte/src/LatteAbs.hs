module LatteAbs where

data LatteTree = LtTop [LatteFun]
    deriving (Eq, Show)
data LatteFun = LtFun LatteId LatteType [LatteArg] LatteStmt
    deriving (Eq, Show)
data LatteType = LtInt
               | LtString
               | LtBool
               | LtVoid
    deriving (Eq, Show)
data LatteId = LtId String
    deriving (Eq, Show)
data LatteStmt = LtBlock [LatteStmt]
    deriving (Eq, Show)
data LatteArg = LtArg LatteId LatteType
    deriving (Eq, Show)
