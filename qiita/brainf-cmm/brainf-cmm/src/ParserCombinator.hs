{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParserCombinator (Parse, parse) where

import Control.Applicative (Alternative(..))
import Data.Maybe (listToMaybe)
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.Char (CharParsing(..))

newtype Parse a = Parse { runParse :: String -> [(a, String)] }

instance Functor Parse where
	fmap f (Parse p) = Parse $ \inp -> [ (f x, r) | (x, r) <- p inp ]

instance Applicative Parse where
	Parse pf <*> Parse px = Parse $ \inp ->
		[ (f x, r') | (f, r) <- pf inp, (x, r') <- px r ]
	pure v = Parse $ \inp -> [(v, inp)]

instance Alternative Parse where
	Parse p1 <|> Parse p2 = Parse $ \inp -> p1 inp ++ p2 inp
	empty = Parse $ \_ -> []

instance Parsing Parse where
	try = id
	(<?>) = const
	notFollowedBy (Parse p) = Parse $ \inp -> case p inp of
		[] -> [((), inp)]
		_ -> []
	unexpected = error
	eof = Parse $ \inp -> case inp of
		"" -> [((), "")]
		_ -> []

instance CharParsing Parse where
	satisfy p = Parse $ \inp -> case inp of
		c : cs | p c -> [(c, cs)]
		_ -> []

parse :: Parse a -> String -> Maybe a
parse p = listToMaybe . map fst . ((p <* eof) `runParse`)
