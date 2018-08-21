{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodeGen (codeGen) where

import Parser

type FunName = String

data Op = PtrInc | PtrDec | ValInc | ValDec | PutCh | GetCh | Call FunName
	deriving Show

codeGen :: String -> Maybe String
codeGen = (funsToCode . toFunctions . ("fun" ,) <$>) . parseBrainfCmm

funsToCode :: [Function] -> String
funsToCode (tf : fs) = header ++ topFun tf ++ concatMap loopFun fs
funsToCode _ = error "funsToCode: no functions"

header :: String
header =
	"section \"data\"\n" ++
	"{\n\tmemory: bits8[30000];\n}\n\n" ++
	"cmm_main()\n{\n\tR2 = memory;\n\t" ++
	"(bits64 r) = call fun();\n\treturn(r);\n}\n\n"

topFun :: Function -> String
topFun f = funName f ++ "()\n{\n" ++
	concatMap toInstruction (funBody f) ++ "\t\treturn();\n}\n\n"

loopFun :: Function -> String
loopFun f = funName f ++ "()\n{\n" ++
	intoLoop (funName f) (concatMap toInstruction $ funBody f) ++ "}\n\n"

intoLoop :: FunName -> String -> String
intoLoop f ops =
	"\tif (bits8[R2] > 0) {\n" ++ ops ++
	"\t\tjump " ++ f ++ "();\n\t" ++
	"} else {\n\t\treturn();\n\t}\n"

toInstruction :: Op -> String
toInstruction PtrInc = "\t\tR2 = R2 + 1;\n"
toInstruction PtrDec = "\t\tR2 = R2 - 1;\n"
toInstruction ValInc = "\t\tbits8[R2] = bits8[R2] + 1;\n"
toInstruction ValDec = "\t\tbits8[R2] = bits8[R2] - 1;\n"
toInstruction PutCh = "\t\tcall putchar_syscall(bits8[R2]);\n"
toInstruction GetCh = "\t\t(bits8 r) = call getchar_syscall(); bits8[R2] = r;\n"
toInstruction (Call fn) = "\t\tcall " ++ fn ++ "();\n"

data Function = Function { funName :: FunName, funBody :: [Op] } deriving Show

toFunctions :: (FunName, ParseForest) -> [Function]
toFunctions (fn, pf) = Function fn ops : concatMap toFunctions fnpfs
	where
	(ops, fnpfs) =
		popFunction (map (((fn ++ "_") ++) . show) [(1 :: Int) ..]) pf

popFunction :: [FunName] -> ParseForest -> ([Op], [(FunName, ParseForest)])
popFunction fns (PtNop : pf) = popFunction fns pf
popFunction (fn : fns) (PtLoop l : pf) = (Call fn : ops, (fn, l) : ls)
	where (ops, ls) = popFunction fns pf
popFunction fns (pt : pf) = (treeToOp pt : ops, ls)
	where (ops, ls) = popFunction fns pf
popFunction _ [] = ([], [])

treeToOp :: ParseTree -> Op
treeToOp PtPtrInc = PtrInc
treeToOp PtPtrDec = PtrDec
treeToOp PtValInc = ValInc
treeToOp PtValDec = ValDec
treeToOp PtPutCh = PutCh
treeToOp PtGetCh = GetCh
treeToOp pt = error $ "treeToOp: Cannnot convert " ++ show pt ++ " to Op"
