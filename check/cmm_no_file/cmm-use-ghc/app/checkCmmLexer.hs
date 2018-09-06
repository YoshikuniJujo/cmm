module Main where

import CheckCmmLexer

main :: IO ()
main = print . ((fromGenLocated <$>) <$>) . getResult $ lexAll dflags0 myPState
