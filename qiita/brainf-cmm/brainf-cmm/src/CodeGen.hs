{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodeGen (codeGen) where

import ParseForest (ParseForest, ParseTree(..), Op(..))

codeGen :: ParseForest -> String
codeGen = toCmm . toFunctions . ("fun" ,)

data OpCall = Op Op | Call FunName deriving Show
type FunName = String

toCmm :: [Function] -> String
toCmm (tf : fs) = header ++ topFun tf ++ concatMap loopFun fs
toCmm [] = error "toCmm: no functions"

header :: String
header =
	"section \"data\"\n{\n\tmemory: bits8[30000];\n}\n\n" ++
	"cmm_main()\n{\n\t" ++
	"R2 = memory;\n\tcall fun();\n\treturn(bits8[R2]);\n}\n\n"

topFun :: Function -> String
topFun Function { funName = fn, funBody = fb } = fn ++ "()\n{\n" ++
	concat (zipWith toInstr [1 ..] fb) ++ "\t\treturn();\n}\n\n"

loopFun :: Function -> String
loopFun Function { funName = fn, funBody = fb } = fn ++ "()\n{\n" ++
	intoLoop fn (concat $ zipWith toInstr [1 ..] fb) ++ "}\n\n"

intoLoop :: FunName -> String -> String
intoLoop fn ops =
	"\tif (bits8[R2] > 0) {\n" ++ ops ++ "\t\tjump " ++ fn ++ "();\n\t" ++
	"} else {\n\t\treturn();\n\t}\n"

toInstr :: Int -> OpCall -> String
toInstr _ (Op PtrInc) = "\t\tif (R2 < memory + 29999) { R2 = R2 + 1; }\n"
toInstr _ (Op PtrDec) = "\t\tif (R2 > memory) { R2 = R2 - 1; }\n"
toInstr _ (Op ValInc) = "\t\tbits8[R2] = bits8[R2] + 1;\n"
toInstr _ (Op ValDec) = "\t\tbits8[R2] = bits8[R2] - 1;\n"
toInstr _ (Op PutCh) = "\t\tcall putchar_syscall(bits8[R2]);\n"
toInstr i (Op GetCh) = "\t\t(bits8 r" ++ show i ++
	") = call getchar_syscall(); bits8[R2] = r" ++ show i ++ ";\n"
toInstr _ (Call fn) = "\t\tcall " ++ fn ++ "();\n"

data Function = Function { funName :: FunName, funBody :: [OpCall] }
	deriving Show

toFunctions :: (FunName, ParseForest) -> [Function]
toFunctions (fn, pf) = Function fn ops : concatMap toFunctions fnpfs
	where
	(ops, fnpfs) = popFun (map (((fn ++ "_") ++) . show) [(1 :: Int) ..]) pf

popFun :: [FunName] -> ParseForest -> ([OpCall], [(FunName, ParseForest)])
popFun fns (PtNop : pf) = popFun fns pf
popFun (fn : fns) (PtLoop l : pf) =
	let (ops, ls) = popFun fns pf in (Call fn : ops, (fn, l) : ls)
popFun fns (PtOp op : pf) = let (ops, ls) = popFun fns pf in (Op op : ops, ls)
popFun _ [] = ([], [])
popFun [] (PtLoop _ : _) = error "popFun: Not enough function names"
