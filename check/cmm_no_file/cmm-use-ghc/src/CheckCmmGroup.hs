{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckCmmGroup where

import System.IO.Unsafe

import DynFlags
import Cmm
import CLabel
import Outputable
import PprCmmDecl
import PprCmm
import Hoopl.Label
import Hoopl.Collections

import Tools

dflags :: DynFlags
dflags = unsafePerformIO myDynFlags

out :: Outputable a => a -> IO ()
out = output dflags

helloL :: CLabel
helloL = mkCmmCodeLabel (thisPackage dflags) "hello"

sample1 :: GenCmmGroup CmmStatics CmmTopInfo Char
sample1 = [
	CmmProc myTopInfo helloL [] 'z',
	CmmData (Section Text helloL) statics1
	]

statics1 :: CmmStatics
statics1 = Statics helloL [
	CmmString [104, 101, 108, 108, 111],
	CmmStaticLit $ CmmInt 123 W8 ]

myTopInfo :: CmmTopInfo
myTopInfo = TopInfo mapEmpty myStackInfo

myStackInfo :: CmmStackInfo
myStackInfo = StackInfo {
	arg_space = 0,
	updfr_space = Nothing,
	do_layout = False }
