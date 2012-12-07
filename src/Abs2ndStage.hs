module Abs2ndStage where

import qualified Data.Map as M

import AbsCommon

type UniqId = String
type LValue = [String]

data Program = Prog (M.Map UniqId Function) (M.Map String Class)
    deriving (Show, Eq)

data Class = Class (Class) (M.Map String Type) (M.Map String Function)
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
    | Ass LValue Expression
    | Incr LValue
    | Decr LValue
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
    | App LValue [Expression]
    | ConstBool Bool
    | ConstInt Int
    | ConstStr String
    | EId LValue
    deriving (Show, Eq)

type Type = LatteType
type IRelation = LatteRel
data EqRelation =
      Eq
    | Neq
    deriving (Show, Eq)
