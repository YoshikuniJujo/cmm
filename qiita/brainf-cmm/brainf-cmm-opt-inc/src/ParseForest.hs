module ParseForest (ParseForest, ParseTree(..), Op(..)) where

type ParseForest = [ParseTree]
data ParseTree = PtNop | PtOp Op | PtLoop ParseForest deriving Show
data Op = PtrInc | PtrDec | ValInc | ValDec | PutCh | GetCh deriving Show
