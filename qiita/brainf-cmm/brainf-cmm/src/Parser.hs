{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Parser (ParseForest, ParseTree(..), parseBrainfCmm) where

import Control.Applicative
import Text.Parser.Combinators
import Text.Parser.Char

import SimpleParser (Parse, parse)

type ParseForest = [ParseTree]

data ParseTree
	= PtPtrInc
	| PtPtrDec
	| PtValInc
	| PtValDec
	| PtPutCh
	| PtGetCh
	| PtLoop ParseForest
	| PtNop
	deriving Show

parseBrainfCmm :: String -> Maybe ParseForest
parseBrainfCmm = parse prsBrainfCmm

prsBrainfCmm :: Parse ParseForest
prsBrainfCmm = some $ choice [
	prsPtrInc, prsPtrDec, prsValInc, prsValDec, prsPutCh, prsGetCh,
	prsLoop, PtNop <$ noneOf ['[', ']'] ]

prsPtrInc, prsPtrDec, prsValInc, prsValDec, prsPutCh, prsGetCh, prsLoop ::
	Parse ParseTree
prsPtrInc = PtPtrInc <$ char '>'
prsPtrDec = PtPtrDec <$ char '<'
prsValInc = PtValInc <$ char '+'
prsValDec = PtValDec <$ char '-'
prsPutCh = PtPutCh <$ char '.'
prsGetCh = PtGetCh <$ char ','
prsLoop = PtLoop <$> (char '[' *> prsBrainfCmm <* char ']')
