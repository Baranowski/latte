module LatteAbs where

import AbsCommon

data Pos = Pos { line :: Int, col :: Int}
    deriving (Eq, Show)
data Located a = Loc Pos a
    deriving (Eq, Show)

data LatteTree = LtTop [Located LatteFun] [Located LatteClass]
    deriving (Eq, Show)
data LatteClass = LtClass LatteId (Maybe LatteId) [Located LatteCDecl] [Located LatteFun]
    deriving (Eq, Show)
data LatteFun = LtFun LatteId LatteType [Located LatteArg] (Located LatteStmt)
    deriving (Eq, Show)
type LatteId = String
type LatteLval = [LatteId]
data LatteArg = LtArg LatteId LatteType
    deriving (Eq, Show)
data LatteStmt = LtSExpr (Located LatteExpr)
               | LtWhile (Located LatteExpr) (Located LatteStmt)
               | LtIf (Located LatteExpr) (Located LatteStmt) (Located LatteStmt)
               | LtIncr LatteLval
               | LtDecr LatteLval
               | LtAss LatteLval (Located LatteExpr)
               | LtDBlock LatteType [Located LatteDecl]
               | LtBlock [Located LatteStmt]
               | LtReturn (Located LatteExpr)
               | LtPass
    deriving (Eq, Show)

data LatteExpr = LtEOr [(Located LatteExpr)]
               | LtEAnd [(Located LatteExpr)]
               | LtERel LatteRel (Located LatteExpr) (Located LatteExpr)
               | LtEAdd (Located LatteExpr) [(LatteAddOp, (Located LatteExpr))]
               | LtEMul (Located LatteExpr) [(LatteMulOp, (Located LatteExpr))]
               | LtENot (Located LatteExpr)
               | LtENeg (Located LatteExpr)
               | LtEStr String
               | LtEApp LatteLval [(Located LatteExpr)]
               | LtEFalse
               | LtETrue
               | LtEInt Int
               | LtEId LatteLval
               | LtENew String
               | LtENull String
               | LtEVoid
    deriving (Eq, Show)
data LatteAddOp = Ladd
                | Lsub
    deriving (Eq, Show)
data LatteMulOp = Lmul
                | Ldiv
                | Lmod
    deriving (Eq, Show)
data LatteDecl = LtDExpr LatteId (Located LatteExpr)
               | LtDEmpty LatteId
    deriving (Eq, Show)
data LatteCDecl = LtCDecl LatteId LatteType
    deriving (Eq, Show)
