{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Parser (
	ParseForest, ParseTree(..), Op(..), parseBrainfCmm ) where

import Control.Applicative
import Text.Parser.Combinators
import Text.Parser.Char

import SimpleParser (Parse, parse)

type ParseForest = [ParseTree]

data ParseTree
	= PtOp Op
	| PtLoop ParseForest
	| PtNop
	deriving Show

data Op = PtrInc | PtrDec | ValInc | ValDec | PutCh | GetCh deriving Show

parseBrainfCmm :: String -> Maybe ParseForest
parseBrainfCmm = parse prsBrainfCmm

prsBrainfCmm :: Parse ParseForest
prsBrainfCmm = some $ choice [
	prsPtrInc, prsPtrDec, prsValInc, prsValDec, prsPutCh, prsGetCh,
	prsLoop, PtNop <$ noneOf ['[', ']'] ]

prsPtrInc, prsPtrDec, prsValInc, prsValDec, prsPutCh, prsGetCh, prsLoop ::
	Parse ParseTree
prsPtrInc = PtOp PtrInc <$ char '>'
prsPtrDec = PtOp PtrDec <$ char '<'
prsValInc = PtOp ValInc <$ char '+'
prsValDec = PtOp ValDec <$ char '-'
prsPutCh = PtOp PutCh <$ char '.'
prsGetCh = PtOp GetCh <$ char ','
prsLoop = PtLoop <$> (char '[' *> prsBrainfCmm <* char ']')
