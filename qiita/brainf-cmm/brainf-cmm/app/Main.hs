{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment (getArgs)
import System.FilePath ((-<.>))
import CodeGen (codeGen)

main :: IO ()
main = do
	src : _ <- getArgs
	maybe (putStrLn "parse error") (writeFile $ src -<.> "cmm") . codeGen
		=<< readFile src
