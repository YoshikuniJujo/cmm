{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseBfSample1 where

import System.IO.Unsafe

import DynFlags
import Cmm
import CmmParse
import CLabel
import Hoopl.Collections
import Hoopl.Label
import Hoopl.Graph
import Hoopl.Block
import BlockId
import Unique
import Module

import Tools
import ToolsCmmGroup

myBfSample1 :: CmmGroup
myBfSample1 = unsafePerformIO $ do
	(_, Just cmm) <- parseCmmFile dflags0 "samples/bfSample1.cmm"
	return cmm

myCmmData0 :: GenCmmDecl CmmStatics CmmTopInfo CmmGraph
myCmmData0 = CmmData
	(Section Data mkCmmDataLabelTPDFMem0)
	(Statics mkCmmDataLabelTPDFMem0 [cmmUninitialised30000_0])

----------------------------------------------------------------------

mkCmmDataLabelTPDFMem1, mkCmmDataLabelTPDFMem2 :: CLabel
cmmUninitialised30000 :: CmmStatic

mkCmmCodeLabelTPDFCmmMain :: CLabel
itsMapEmpty :: LabelMap CmmInfoTable
cmmBlockId1 :: BlockId
cmmMainGManyCenter :: Body CmmNode
[
	CmmData	(Section Data mkCmmDataLabelTPDFMem1)
		(Statics mkCmmDataLabelTPDFMem2 [cmmUninitialised30000]),
	CmmProc	(TopInfo itsMapEmpty (StackInfo 8 (Just 8) True))
		mkCmmCodeLabelTPDFCmmMain
		[]
		(CmmGraph
			cmmBlockId1
			(GMany	NothingO cmmMainGManyCenter NothingO))
	] = myBfSample1

mkCmmDataLabelTPDFMem0 :: CLabel
mkCmmDataLabelTPDFMem0 = mkCmmDataLabel (thisPackage dflags0) "memory"
cmmUninitialised30000_0 :: CmmStatic
cmmUninitialised30000_0 = CmmUninitialised 30000

itsMapEmpty0 :: LabelMap CmmInfoTable
itsMapEmpty0 = mapEmpty
mkCmmCodeLabelTPDFCmmMain0 :: CLabel
mkCmmCodeLabelTPDFCmmMain0 = mkCmmCodeLabel (thisPackage dflags0) "cmm_main"
cmmBlockId0 :: BlockId
cmmBlockId0 = mkBlockId unique1

----------------------------------------------------------------------

cmmBlockId2, cmmBlockId3 :: BlockId
cmmMainBody :: Block CmmNode O O
gcWordDF1 :: CmmType
[(	cmmBlockId2,
	(BlockCC
		(CmmEntry cmmBlockId3 GlobalScope)
		cmmMainBody
		(CmmCall
			(CmmLoad (CmmStackSlot Old 8) gcWordDF1)
			Nothing
			[VanillaReg 1 VNonGcPtr]
			8 0 8)) )] = mapToList cmmMainGManyCenter

gcWordDF0 :: CmmType
gcWordDF0 = gcWord dflags0

----------------------------------------------------------------------

cmmMainBodyList :: [CmmNode O O]
cmmMainInitialize, cmmMainIncrementValue, cmmMainIncrementPointer,
	cmmMainDecrementPointer, cmmMainReturnValue :: CmmNode O O
cmmMainBodyList@[
	cmmMainInitialize,
	cmmMainIncrementValue,
	_,
	cmmMainIncrementPointer,
	_,
	_,
	_,
	_,
	cmmMainDecrementPointer,
	cmmMainReturnValue
	] = filter notCmmTick $ bSnocToList cmmMainBody

cmmMainInitialize0 :: CmmNode O O
cmmMainInitialize0 = CmmAssign
	(CmmGlobal (VanillaReg 2 VNonGcPtr))
	(CmmLit (CmmLabel mkCmmCodeLabelRtsUnitIdMemory0))
cmmMainIncrementValue0 :: CmmNode O O
cmmMainIncrementValue0 = CmmStore
	(CmmReg (CmmGlobal (VanillaReg 2 VNonGcPtr)))
	(CmmMachOp
		(MO_Add W8)
		[	CmmLoad
				(CmmReg (CmmGlobal (VanillaReg 2 VNonGcPtr)))
				itsB8_0,
			CmmLit (CmmInt 1 W64) ])

----------------------------------------------------------------------

mkCmmCodeLabelRtsUnitIdMemory :: CLabel
CmmAssign
	(CmmGlobal (VanillaReg 2 VNonGcPtr))
	(CmmLit (CmmLabel mkCmmCodeLabelRtsUnitIdMemory)) = cmmMainInitialize
itsB8 :: CmmType
CmmStore
	(CmmReg (CmmGlobal (VanillaReg 2 VNonGcPtr)))
	(CmmMachOp
		(MO_Add W8)
		[	CmmLoad
				(CmmReg (CmmGlobal (VanillaReg 2 VNonGcPtr)))
				itsB8,
			CmmLit (CmmInt 1 W64) ]) = cmmMainIncrementValue

mkCmmCodeLabelRtsUnitIdMemory0 :: CLabel
mkCmmCodeLabelRtsUnitIdMemory0 = mkCmmCodeLabel rtsUnitId "memory"
itsB8_0 :: CmmType
itsB8_0 = b8

----------------------------------------------------------------------

unique1 :: Unique
unique1 : _ = unsafePerformIO $ getMyUniqueList 10
