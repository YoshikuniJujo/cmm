{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckCmmParse where

import System.IO.Unsafe

import Cmm
import CmmParse
import CLabel
import Hoopl.Label
import Hoopl.Collections
import SMRep
import DynFlags

import Tools

myCmmParsed :: CmmGroup
myCmmParsed = unsafePerformIO $ do
	(_, Just cmm) <- parseCmmFile dflags0 "samples/sample.cmm"
	return cmm

myTopInfo :: CmmTopInfo
myCLabel :: CLabel
myGlobalRegs :: [GlobalReg]
myGraph :: CmmGraph
[CmmProc myTopInfo myCLabel myGlobalRegs myGraph] = myCmmParsed

myInfoTbls :: LabelMap CmmInfoTable
myStackInfo :: CmmStackInfo
TopInfo myInfoTbls myStackInfo = myTopInfo

{-
mapSize myInfoTbls == 0

myCitLbl :: CLabel
myCitRep :: SMRep
myCitProf :: ProfilingInfo
myCitSrt :: C_SRT
CmmInfoTable myCitLbl myCitRep myCitProf myCitSrt = myInfoTbls
-}

myArgSpace :: ByteOff
myUpdfrSpace :: Maybe ByteOff
myDoLayout :: Bool
StackInfo myArgSpace myUpdfrSpace myDoLayout = myStackInfo

myCLabel0 :: CLabel
myCLabel0 = mkCmmCodeLabel (thisPackage dflags0) "cmm_main"
