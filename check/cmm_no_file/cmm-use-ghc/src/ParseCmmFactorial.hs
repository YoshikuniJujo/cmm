{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseCmmFactorial where

import System.IO.Unsafe

import DynFlags
import Cmm
import CmmParse
import CLabel
import Hoopl.Collections
import Hoopl.Graph
import Hoopl.Block
import BlockId

import Tools
import ToolsCmmGroup

myCmmFactorial :: CmmGroup
myCmmFactorial = unsafePerformIO $ do
	(_, Just cmm) <- parseCmmFile dflags0 "samples/factorial.cmm"
	return cmm

----------------------------------------------------------------------

-- myCmmMain, myCmmFact :: GenCmmDecl CmmStatics CmmTopInfo CmmGraph
mainTopInfo, factTopInfo :: CmmTopInfo
mainCLabel, factCLabel :: CLabel
mainGlobalRegs, factGlobalRegs :: [GlobalReg]
mainGraph, factGraph :: CmmGraph
[
	CmmProc mainTopInfo mainCLabel mainGlobalRegs mainGraph,
	CmmProc factTopInfo factCLabel factGlobalRegs factGraph
	] = myCmmFactorial

mainTopInfo0, factTopInfo0 :: CmmTopInfo
mainTopInfo0 = TopInfo mapEmpty (StackInfo 8 (Just 8) True)
factTopInfo0 = mainTopInfo0
mainCLabel0, factCLabel0 :: CLabel
mainCLabel0 = mkCmmCodeLabel (thisPackage dflags0) "cmm_main"
factCLabel0 = mkCmmCodeLabel (thisPackage dflags0) "factorial"
mainGlobalRegs0, factGlobalRegs0 :: [GlobalReg]
mainGlobalRegs0 = []
factGlobalRegs0 = []

----------------------------------------------------------------------

mainGEntry, factGEntry :: BlockId
mainGGraph, factGGraph :: Graph CmmNode C C
CmmGraph mainGEntry mainGGraph = mainGraph
CmmGraph factGEntry factGGraph = factGraph
