module AbsCommon where

data LatteType = LtInt
               | LtString
               | LtBool
               | LtVoid
               | LtType String
    deriving (Eq)
instance Show LatteType where
    show LtInt = "int"
    show LtString = "string"
    show LtBool = "bool"
    show LtVoid = "void"
    show (LtType s) = s

data LatteRel = Rlt
              | Rle
              | Rgt
              | Rge
              | Req
              | Rne
    deriving (Eq, Show)
