{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckLexer where

import Prelude hiding (lex)

import Lexer
import SrcLoc
import StringBuffer
import Outputable

import Tools

lex :: PState -> ParseResult (Located Token)
lex = unP $ lexer True pure

lexAll :: PState -> ParseResult [Located Token]
lexAll = unP go
	where
	go = do
		t <- lexer True pure
		case t of
			L _ ITeof -> return []
			_ -> (t :) <$> go

myPState :: PState
myPState = mkPState
	dflags0
	(stringToStringBuffer "main = putStrLn \"hello\"")
	(mkRealSrcLoc "foo.hs" 0 0)

getResult :: ParseResult a -> Maybe a
getResult = \case POk _ x -> Just x; _ -> Nothing

outResult :: Outputable a => ParseResult a -> IO ()
outResult = out . getResult
