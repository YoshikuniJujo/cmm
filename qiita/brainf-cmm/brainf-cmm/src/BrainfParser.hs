{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BrainfParser (ParseForest, ParseTree(..), Op(..), prsBrainf) where

import Control.Applicative (some)
import Text.Parser.Combinators (choice)
import Text.Parser.Char (CharParsing, char, noneOf)

import ParseForest (ParseForest, ParseTree(..), Op(..))

prsBrainf :: CharParsing p => p ParseForest
prsBrainf = some $ choice [
	prsNop, prsPtrInc, prsPtrDec, prsValInc, prsValDec,
	prsPutCh, prsGetCh, prsLoop ]

prsNop, prsPtrInc, prsPtrDec, prsValInc, prsValDec,
	prsPutCh, prsGetCh, prsLoop :: CharParsing p => p ParseTree
prsNop = PtNop <$ noneOf "><+-.,[]"
prsPtrInc = PtOp PtrInc <$ char '>'
prsPtrDec = PtOp PtrDec <$ char '<'
prsValInc = PtOp ValInc <$ char '+'
prsValDec = PtOp ValDec <$ char '-'
prsPutCh = PtOp PutCh <$ char '.'
prsGetCh = PtOp GetCh <$ char ','
prsLoop = PtLoop <$> (char '[' *> prsBrainf <* char ']')
