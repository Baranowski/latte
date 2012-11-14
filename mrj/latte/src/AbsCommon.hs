module AbsCommon where

-- TODO: wypisywanie typu
data LatteType = LtInt
               | LtString
               | LtBool
               | LtVoid
    deriving (Eq, Show)

data LatteRel = Rlt
              | Rle
              | Rgt
              | Rge
              | Req
              | Rne
    deriving (Eq, Show)
