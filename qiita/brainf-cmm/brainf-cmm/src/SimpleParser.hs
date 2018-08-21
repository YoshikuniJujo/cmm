{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SimpleParser (Parse, parse) where

import Control.Applicative (Alternative(..))
import Data.Maybe (listToMaybe)
import Text.Parser.Combinators
import Text.Parser.Char

newtype Parse a = Parse { runParse :: String -> [(a, String)] }

parse :: Parse a -> String -> Maybe a
parse p = listToMaybe . map fst . ((p <* eof) `runParse`)

succeed :: a -> Parse a
succeed v = Parse $ \inp -> [(v, inp)]

check :: (Char -> Bool) -> Parse Char
check p = Parse $ \inp -> case inp of
	c : cs | p c -> [(c, cs)]
	_ -> []

instance Functor Parse where
	fmap f (Parse p) = Parse $ \inp -> [ (f x, r) | (x, r) <- p inp ]

instance Applicative Parse where
	Parse pf <*> Parse px = Parse $ \inp ->
		[ (f x, r') | (f, r) <- pf inp, (x, r') <- px r ]
	pure = succeed

instance Alternative Parse where
	Parse p1 <|> Parse p2 = Parse $ \inp -> p1 inp ++ p2 inp
	empty = Parse $ \_ -> []

pEof :: Parse ()
pEof = Parse $ \inp -> case inp of
	"" -> [((), "")]
	_ -> []

pNotFollowedBy :: Show a => Parse a -> Parse ()
pNotFollowedBy (Parse p) = Parse $ \inp -> case p inp of
	[] -> [((), inp)]
	_ -> []

instance Parsing Parse where
	try = id
	(<?>) = const
	notFollowedBy = pNotFollowedBy
	unexpected = error
	eof = pEof

instance CharParsing Parse where
	satisfy = check
