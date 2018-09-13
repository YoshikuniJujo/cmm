{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
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
import Hoopl.Label
import BlockId
import Unique
import CoreSyn
import SrcLoc

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

----------------------------------------------------------------------

mainGManyCenter, factGManyCenter :: Body CmmNode
GMany NothingO mainGManyCenter NothingO = mainGGraph
GMany NothingO factGManyCenter NothingO = factGGraph

----------------------------------------------------------------------

mainReturnLabel, mainCallLabel :: Label
mainReturnBlock, mainCallBlock :: Block CmmNode C C
[	(mainReturnLabel, mainReturnBlock),
	(mainCallLabel, mainCallBlock) ] = mapToList mainGManyCenter

mainReturnBlock0, mainCallBlock0 :: Block CmmNode C C
mainReturnBlock0 = BlockCC mainReturnEntry0 mainReturnAssign0 mainReturnCall0
mainCallBlock0 = undefined

factRecLabel, factLoopLabel, factReturnLabel, factIfLabel :: Label
factRecBlock, factLoopBlock, factReturnBlock, factIfBlock :: Block CmmNode C C
[	(factRecLabel, factRecBlock),
	(factLoopLabel, factLoopBlock),
	(factReturnLabel, factReturnBlock),
	(factIfLabel, factIfBlock) ] = mapToList factGManyCenter

----------------------------------------------------------------------

mainReturnEntry :: CmmNode C O
mainReturnAssign :: Block CmmNode O O
mainReturnCall :: CmmNode O C
BlockCC mainReturnEntry mainReturnAssign mainReturnCall = mainReturnBlock

mainReturnEntry0 :: CmmNode C O
mainReturnEntry0 = CmmEntry mainReturnEntryLabel0 GlobalScope
mainReturnAssign0 :: Block CmmNode O O
mainReturnAssign0 =
	BSnoc (BMiddle mainReturnAssignToMem0) mainReturnAssignFromMem0
mainReturnCall0 :: CmmNode O C
mainReturnCall0 = CmmCall
	(CmmLoad (CmmStackSlot Old 8) $ gcWord dflags0)
	Nothing [VanillaReg 1 VNonGcPtr] 8 0 8

mainCallEntry :: CmmNode C O
mainCallAssign :: Block CmmNode O O
mainCallCall :: CmmNode O C
BlockCC mainCallEntry mainCallAssign mainCallCall = mainCallBlock

mainCallEntry0 :: CmmNode C O
mainCallEntry0 = CmmEntry mainCallEntryLabel0 GlobalScope

----------------------------------------------------------------------

mainReturnEntryLabel :: Label
CmmEntry mainReturnEntryLabel GlobalScope = mainReturnEntry
mainReturnAssignToMem :: CmmNode O O
mainReturnAssignFromMem :: CmmNode O O
BSnoc (BMiddle mainReturnAssignToMem) mainReturnAssignFromMem = mainReturnAssign
gcWordDflags0 :: CmmType
CmmCall	(CmmLoad (CmmStackSlot Old 8) gcWordDflags0)
	Nothing
	[VanillaReg 1 VNonGcPtr] 8 0 8 = mainReturnCall

mainReturnAssignToMem0, mainReturnAssignFromMem0 :: CmmNode O O
mainReturnAssignToMem0 = CmmAssign
	(CmmLocal (LocalReg unique1 b64))
	(CmmReg (CmmGlobal (VanillaReg 1 VNonGcPtr)))
mainReturnAssignFromMem0 = CmmAssign
	(CmmGlobal (VanillaReg 1 VNonGcPtr))
	(CmmReg (CmmLocal (LocalReg unique1 b64)))

realSrcSpanFact2152 :: RealSrcSpan
mainReturnEntryBId :: BlockId
mainReturnEntryBId2 :: BlockId
BSnoc	(BSnoc	(BSnoc	(BMiddle
				(CmmTick
					(SourceNote
						realSrcSpanFact2152
						"cmm_main")))
				(CmmStore
					(CmmStackSlot
						(Young mainReturnEntryBId)
						8)
					(CmmLit
						(CmmBlock
							mainReturnEntryBId2))))
		(CmmAssign
			(CmmGlobal (VanillaReg 2 VNonGcPtr))
			(CmmLit (CmmInt 1 W64))))
	(CmmAssign
		(CmmGlobal (VanillaReg 1 VNonGcPtr))
		(CmmLit (CmmInt 10 W64))) =
	mainCallAssign

----------------------------------------------------------------------

itsB64, itsB64_2 :: CmmType
CmmAssign
	(CmmLocal (LocalReg _ itsB64))
	(CmmReg (CmmGlobal (VanillaReg 1 VNonGcPtr))) = mainReturnAssignToMem
CmmAssign
	(CmmGlobal (VanillaReg 1 VNonGcPtr))
	(CmmReg (CmmLocal (LocalReg _ itsB64_2))) = mainReturnAssignFromMem

----------------------------------------------------------------------

mainReturnEntryLabel0, mainCallEntryLabel0 :: Label
mainReturnEntryLabel0 = mkBlockId unique0
mainCallEntryLabel0 = mkBlockId unique2
unique0, unique1, unique2 :: Unique
unique0 : unique1 : unique2 : _ = unsafePerformIO $ getMyUniqueList 10
