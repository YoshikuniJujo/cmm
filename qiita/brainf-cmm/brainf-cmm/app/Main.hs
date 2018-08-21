{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.FilePath

import CodeGen

main :: IO ()
main = do
	src : _ <- getArgs
	cnt <- readFile src
	case codeGen cnt of
		Nothing -> putStrLn "parse error"
		Just cmm -> writeFile (src -<.> "cmm") cmm
