module Main where

import System.Environment (getArgs)
import System.FilePath ((-<.>))
import CodeGen (codeGen)
import BrainfParser (prsBrainfCmm)
import ParserCombinator (parse)

main :: IO ()
main = do
	src : _ <- getArgs
	maybe (putStrLn "parse error") (writeFile $ src -<.> "cmm")
		. (codeGen <$>) . parse prsBrainfCmm =<< readFile src
