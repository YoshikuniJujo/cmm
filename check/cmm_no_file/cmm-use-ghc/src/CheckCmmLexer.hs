{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckCmmLexer (
	dflags0, lexAll, myPState, fromGenLocated, getResult) where

import Prelude hiding (lex)

import CmmLex
import CmmMonad
import Lexer
import SrcLoc
import DynFlags
import StringBuffer

import Tools

{-
lex :: DynFlags -> PState -> ParseResult (Located CmmToken)
lex = unPD $ cmmlex pure
-}

lexAll :: DynFlags -> PState -> ParseResult [Located CmmToken]
lexAll = unPD go
	where
	go = do
		t <- cmmlex pure
		case t of
			L _ (CmmT_EOF) -> return []
			_ -> (t :) <$> go

myPState :: PState
myPState = (mkPState
	dflags0
	(stringToStringBuffer "cmm_main() { return(123); }")
	(mkRealSrcLoc "foo.cmm" 0 0)) { lex_state = [0] }

fromGenLocated :: GenLocated l e -> e
fromGenLocated (L _ e) = e
