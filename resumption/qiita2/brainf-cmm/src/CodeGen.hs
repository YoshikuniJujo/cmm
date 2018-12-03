{-# LANGUAGE TupleSections, QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodeGen (codeGen) where

import Text.Nowdoc
import ParseForest (ParseForest, ParseTree(..), Op(..))

data Function = Function { funName :: FunName, funBody :: [OpCall] }
	deriving Show
data OpCall = Op Op | Call FunName deriving Show
type FunName = String

popFun :: [FunName] -> ParseForest -> ([OpCall], [(FunName, ParseForest)])
popFun fns (PtNop : pf) = popFun fns pf
popFun (fn : fns) (PtLoop l : pf) =
	let (ops, ls) = popFun fns pf in (Call fn : ops, (fn, l) : ls)
popFun fns (PtOp op : pf) = let (ops, ls) = popFun fns pf in (Op op : ops, ls)
popFun _ [] = ([], [])
popFun [] (PtLoop _ : _) = error "popFun: Not enough function names"

toFunctions :: (FunName, ParseForest) -> [Function]
toFunctions (fn, pf) = Function fn ops : concatMap toFunctions fnpfs
	where
	(ops, fnpfs) = popFun (map (((fn ++ "_") ++) . show) [(1 :: Int) ..]) pf

toInstr :: Int -> OpCall -> String
toInstr _ (Op PtrInc) =
	"\t\tif (R2 < memory + 29999) { R2 = R2 + 1; } else { R2 = memory; }\n"
toInstr _ (Op PtrDec) =
	"\t\tif (R2 > memory) { R2 = R2 - 1; } else { R2 = memory + 29999; }\n"
toInstr _ (Op ValInc) = "\t\tbits8[R2] = bits8[R2] + 1;\n"
toInstr _ (Op ValDec) = "\t\tbits8[R2] = bits8[R2] - 1;\n"
toInstr _ (Op PutCh) = "\t\tcall putchar_syscall(bits8[R2]);\n"
toInstr i (Op GetCh) = "\t\t(bits8 r" ++ show i ++
	") = call getchar_syscall(); bits8[R2] = r" ++ show i ++ ";\n"
toInstr _ (Call fn) = "\t\tcall " ++ fn ++ "();\n"

topFun :: Function -> String
topFun Function { funName = fn, funBody = fb } = fn ++
	"()\n{\n" ++ concat (zipWith toInstr [1 ..] fb) ++ "\t\treturn();\n}\n\n"

loopFun :: Function -> String
loopFun Function { funName = fn, funBody = fb } = fn ++ "()\n{\n" ++
	intoLoop fn (concat $ zipWith toInstr [1 ..] fb) ++ "}\n\n"

intoLoop :: FunName -> String -> String
intoLoop fn ops =
	"\tif (bits8[R2] > 0) {\n" ++ ops ++ "\t\tjump " ++ fn ++ "();\n\t" ++
	"} else {\n\t\treturn();\n\t}\n"

header :: String
header = [nowdoc|
section "data"
{
	memory: bits8[30000];
}

cmm_main()
{
	R2 = memory;
	call fun();
	return(bits8[R2]);
}

|]

codeGen :: ParseForest -> String
codeGen = toCmm . toFunctions . ("fun" ,)

toCmm :: [Function] -> String
toCmm (tf : fs) = header ++ topFun tf ++ concatMap loopFun fs
toCmm [] = error "toCmm: no functions"
