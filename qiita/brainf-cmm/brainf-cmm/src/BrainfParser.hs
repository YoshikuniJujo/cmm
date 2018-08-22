{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BrainfParser (
	ParseForest, ParseTree(..), Op(..), parseBrainf ) where

import Control.Applicative (some)
import Text.Parser.Combinators (choice)
import Text.Parser.Char (char, noneOf)

import ParserCombinator (Parse, parse)

type ParseForest = [ParseTree]
data ParseTree = PtNop | PtOp Op | PtLoop ParseForest deriving Show
data Op = PtrInc | PtrDec | ValInc | ValDec | PutCh | GetCh deriving Show

parseBrainf :: String -> Maybe ParseForest
parseBrainf = parse prsBrainf

prsBrainf :: Parse ParseForest
prsBrainf = some $ choice [
	prsNop, prsPtrInc, prsPtrDec, prsValInc, prsValDec,
	prsPutCh, prsGetCh, prsLoop ]

prsNop, prsPtrInc, prsPtrDec, prsValInc, prsValDec,
	prsPutCh, prsGetCh, prsLoop :: Parse ParseTree
prsNop = PtNop <$ noneOf "><+-.,[]"
prsPtrInc = PtOp PtrInc <$ char '>'
prsPtrDec = PtOp PtrDec <$ char '<'
prsValInc = PtOp ValInc <$ char '+'
prsValDec = PtOp ValDec <$ char '-'
prsPutCh = PtOp PutCh <$ char '.'
prsGetCh = PtOp GetCh <$ char ','
prsLoop = PtLoop <$> (char '[' *> prsBrainf <* char ']')
