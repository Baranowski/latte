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
data LatteArg = LtArg LatteId LatteType
    deriving (Eq, Show)
data LatteStmt = LtSExpr LatteExpr
               | LtWhile LatteExpr LatteStmt
               | LtIf LatteExpr LatteStmt LatteStmt
               | LtIncr LatteId
               | LtDecr LatteId
               | LtAss LatteId LatteExpr
               | LtDBlock LatteType [LatteDecl]
               | LtBlock [LatteStmt]
               | LtReturn LatteExpr
               | LtPass
    deriving (Eq, Show)
data LatteExpr = LtEOr [LatteExpr]
               | LtEAnd [LatteExpr]
               | LtERel LatteRel LatteExpr LatteExpr
               | LtEAdd LatteExpr [(LatteAddOp, LatteExpr)]
               | LtEMul LatteExpr [(LatteMulOp, LatteExpr)]
               | LtENot LatteExpr
               | LtENeg LatteExpr
               | LtEStr String
               | LtEApp LatteId [LatteExpr]
               | LtEFalse
               | LtETrue
               | LtEInt Int
               | LtEId LatteId
               | LtEVoid
    deriving (Eq, Show)
data LatteRel = Rlt
              | Rle
              | Rgt
              | Rge
              | Req
              | Rne
    deriving (Eq, Show)
data LatteAddOp = Ladd
                | Lsub
    deriving (Eq, Show)
data LatteMulOp = Lmul
                | Ldiv
                | Lmod
    deriving (Eq, Show)
data LatteDecl = LtDExpr LatteId LatteExpr
               | LtDEmpty LatteId
    deriving (Eq, Show)
