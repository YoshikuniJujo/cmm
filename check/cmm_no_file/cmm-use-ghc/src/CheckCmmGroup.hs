module CheckCmmGroup where

import System.IO.Unsafe

import DynFlags
import Cmm
import CLabel
import PprCmmDecl

import Tools

dflags :: DynFlags
dflags = unsafePerformIO myDynFlags

sample1 :: GenCmmGroup Bool Int Char
sample1 = []
