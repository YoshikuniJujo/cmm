{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CheckCmmParse where

import System.IO.Unsafe

import Cmm
import CmmParse
import CLabel
import Hoopl.Label
import Hoopl.Collections
import Hoopl.Graph
import Hoopl.Block
import BlockId
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

myTopInfo0 :: CmmTopInfo
myTopInfo0 = TopInfo myInfoTbls0 myStackInfo0
myCLabel0 :: CLabel
myCLabel0 = mkCmmCodeLabel (thisPackage dflags0) "cmm_main"
myGlobalRegs0 :: [GlobalReg]
myGlobalRegs0 = []

------------------------------------------------------------------

myInfoTbls :: LabelMap CmmInfoTable
myStackInfo :: CmmStackInfo
TopInfo myInfoTbls myStackInfo = myTopInfo

myInfoTbls0 :: LabelMap CmmInfoTable
myInfoTbls0 = mapEmpty
myStackInfo0 :: CmmStackInfo
myStackInfo0 = StackInfo 8 (Just 8) True

myGEntry :: BlockId
myGGraph :: Graph CmmNode C C
CmmGraph myGEntry myGGraph = myGraph

myGEntry0 :: BlockId
myGEntry0 = unsafePerformIO $ getMyBlockId

-------------------------------------------------------------------

myArgSpace :: ByteOff
myUpdfrSpace :: Maybe ByteOff
myDoLayout :: Bool
StackInfo myArgSpace myUpdfrSpace myDoLayout = myStackInfo
