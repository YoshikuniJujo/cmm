{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParserCombinator (Parse, parse) where

import Control.Applicative (Alternative(..))
import Data.Maybe (listToMaybe)
import Text.Parser.Combinators (Parsing(..))
import Text.Parser.Char (CharParsing(..))

newtype Parse a = Parse { runParse :: String -> [(a, String)] }

parse :: Parse a -> String -> Maybe a
parse p = listToMaybe . map fst . ((p <* eof) `runParse`)

instance Functor Parse where
	fmap f (Parse p) = Parse $ \inp -> [ (f x, r) | (x, r) <- p inp ]

instance Applicative Parse where
	pure v = Parse $ \inp -> [(v, inp)]
	Parse pf <*> Parse px = Parse $ \inp ->
		[ (f x, r') | (f, r) <- pf inp, (x, r') <- px r ]

instance Alternative Parse where
	empty = Parse $ \_ -> []
	Parse p1 <|> Parse p2 = Parse $ \inp -> p1 inp ++ p2 inp

instance Parsing Parse where
	try = id
	(<?>) = const
	unexpected _ = Parse $ \_ -> []
	eof = Parse $ \case "" -> [((), "")]; _ -> []
	notFollowedBy (Parse p) = Parse $ \inp -> case p inp of
		[] -> [((), inp)]
		_ -> []

instance CharParsing Parse where
	satisfy p = Parse $ \case c : cs | p c -> [(c, cs)]; _ -> []
