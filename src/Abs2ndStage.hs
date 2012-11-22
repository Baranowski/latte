module Abs2ndStage where

import qualified Data.Map as M

import AbsCommon

type UniqId = String

data Program = Prog (M.Map UniqId Function)
    deriving (Show, Eq)

-- typ, argumenty, zmienne lokalne, blok
data Function = Func Type [Declaration] [Declaration] Statement
    deriving (Show, Eq)

data Declaration = Decl Type UniqId
    deriving (Show, Eq)

data Statement =
      Blck [Statement]
    | Ret
    | RetExpr Expression
    | Ass UniqId Expression
    | Incr UniqId
    | Decr UniqId
    | If Expression Statement
    | IfElse Expression Statement Statement
    | While Expression Statement
    | SExpr Expression
    | SEmpty
    | Pass
    deriving (Show, Eq)

data Expression =
      Or [Expression]
    | And [Expression]
    | IntComp IRelation Expression Expression
    | StrComp EqRelation Expression Expression
    | BoolComp EqRelation Expression Expression
    | Arithm Char Expression Expression
    | Not Expression
    | Neg Expression
    | Concat Expression Expression
    | App UniqId [Expression]
    | ConstBool Bool
    | ConstInt Int
    | ConstStr String
    | EId UniqId
    deriving (Show, Eq)

type Type = LatteType
type IRelation = LatteRel
data EqRelation =
      Eq
    | Neq
    deriving (Show, Eq)
