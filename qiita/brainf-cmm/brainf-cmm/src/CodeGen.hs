{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodeGen (codeGen) where

import Parser

type FunName = String

data OpCall = Op Op | Call FunName deriving Show

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
	concatMap toInstruction (funBody f) ++ "\t\treturn(bits8[R2]);\n}\n\n"

loopFun :: Function -> String
loopFun f = funName f ++ "()\n{\n" ++
	intoLoop (funName f) (concatMap toInstruction $ funBody f) ++ "}\n\n"

intoLoop :: FunName -> String -> String
intoLoop f ops =
	"\tif (bits8[R2] > 0) {\n" ++ ops ++
	"\t\tjump " ++ f ++ "();\n\t" ++
	"} else {\n\t\treturn();\n\t}\n"

toInstruction :: OpCall -> String
toInstruction (Op PtrInc) = "\t\tif (R2 < memory + 29999) { R2 = R2 + 1; }\n"
toInstruction (Op PtrDec) = "\t\tif (R2 > memory) { R2 = R2 - 1; }\n"
toInstruction (Op ValInc) = "\t\tbits8[R2] = bits8[R2] + 1;\n"
toInstruction (Op ValDec) = "\t\tbits8[R2] = bits8[R2] - 1;\n"
toInstruction (Op PutCh) = "\t\tcall putchar_syscall(bits8[R2]);\n"
toInstruction (Op GetCh) =
	"\t\t(bits8 r) = call getchar_syscall(); bits8[R2] = r;\n"
toInstruction (Call fn) = "\t\tcall " ++ fn ++ "();\n"

data Function = Function { funName :: FunName, funBody :: [OpCall] }
	deriving Show

toFunctions :: (FunName, ParseForest) -> [Function]
toFunctions (fn, pf) = Function fn ops : concatMap toFunctions fnpfs
	where
	(ops, fnpfs) =
		popFunction (map (((fn ++ "_") ++) . show) [(1 :: Int) ..]) pf

popFunction :: [FunName] -> ParseForest -> ([OpCall], [(FunName, ParseForest)])
popFunction fns (PtNop : pf) = popFunction fns pf
popFunction (fn : fns) (PtLoop l : pf) = (Call fn : ops, (fn, l) : ls)
	where (ops, ls) = popFunction fns pf
popFunction fns (PtOp op : pf) = (Op op : ops, ls)
	where (ops, ls) = popFunction fns pf
popFunction _ [] = ([], [])
popFunction [] (PtLoop _ : _) = error "popFunction: function name missing"
