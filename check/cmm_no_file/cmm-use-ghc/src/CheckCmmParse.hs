{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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
import CoreSyn
import SrcLoc
import CmmExpr

import Tools

myCmmParsed :: CmmGroup
myCmmParsed = unsafePerformIO $ do
	(_, Just cmm) <- parseCmmFile dflags0 "samples/sample.cmm"
	return cmm

myCmmParsed0 :: CmmGroup
myCmmParsed0 = [CmmProc myTopInfo0 myCLabel0 myGlobalRegs0 myGraph0]

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
myGraph0 :: CmmGraph
myGraph0 = CmmGraph myGEntry0 myGGraph0

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

myGGraph0 :: Graph CmmNode C C
myGGraph0 = GMany myGManyLeft0 myGManyCenter0 myGManyRight0

-------------------------------------------------------------------

myArgSpace :: ByteOff
myUpdfrSpace :: Maybe ByteOff
myDoLayout :: Bool
StackInfo myArgSpace myUpdfrSpace myDoLayout = myStackInfo

myGManyLeft :: MaybeO C (Block CmmNode O C)
myGManyRight :: MaybeO C (Block CmmNode C O)
myGManyCenter :: Body CmmNode
GMany myGManyLeft myGManyCenter myGManyRight = myGGraph

myGManyLeft0 :: MaybeO C (Block CmmNode O C)
myGManyLeft0 = NothingO
myGManyRight0 :: MaybeO C (Block CmmNode C O)
myGManyRight0 = NothingO
myGManyCenter0 :: Body CmmNode
myGManyCenter0 = mapFromList [(myLabel0, myBody0)]

--------------------------------------------------------------------

myLabel :: Label
myBody :: Block CmmNode C C
[(myLabel, myBody)] = mapToList myGManyCenter

myBody0 :: Block CmmNode C C
myBody0 = BlockCC myBodyLeft0 myBodyCenter0 myBodyRight0

myLabel0 :: Label
myLabel0 = myGEntry0

---------------------------------------------------------------------

myBodyLeft :: CmmNode C O
myBodyRight :: CmmNode O C
myBodyCenter :: Block CmmNode O O
BlockCC myBodyLeft myBodyCenter myBodyRight = myBody

myBodyLeft0 :: CmmNode C O
myBodyLeft0 = CmmEntry myLabel0 GlobalScope

myBodyRight0 :: CmmNode O C
myBodyRight0 = CmmCall myCmlTarget0 myCmlCont0
	myCmlArgsRegs0 myCmlArgs0 myCmlRetArgs0 myCmlRetOff0

myBodyCenter0 :: Block CmmNode O O
myBodyCenter0 = BSnoc (BMiddle myBodyCenterLeft0) myBodyCenterRight0

---------------------------------------------------------------------

myCmlTarget :: CmmExpr
myCmlCont :: Maybe Label
myCmlArgsRegs :: [GlobalReg]
myCmlArgs :: ByteOff
myCmlRetArgs :: ByteOff
myCmlRetOff :: ByteOff
CmmCall myCmlTarget myCmlCont
	myCmlArgsRegs myCmlArgs myCmlRetArgs myCmlRetOff = myBodyRight

myCmlTarget0 :: CmmExpr
myCmlTarget0 = CmmLoad myCmmExprOldP0 myCmmType0
myCmlCont0 :: Maybe Label
myCmlCont0 = Nothing
myCmlArgsRegs0 :: [GlobalReg]
myCmlArgsRegs0 = [VanillaReg 1 VNonGcPtr]
myCmlArgs0, myCmlRetArgs0, myCmlRetOff0 :: ByteOff
myCmlArgs0 = 8
myCmlRetArgs0 = 0
myCmlRetOff0 = 8

myBodyCenterLeft :: CmmNode O O
myBodyCenterRight :: CmmNode O O
BSnoc (BMiddle myBodyCenterLeft) myBodyCenterRight = myBodyCenter

myBodyCenterLeft0 :: CmmNode O O
myBodyCenterLeft0 = CmmTick myCmmTickish0
myBodyCenterRight0 :: CmmNode O O
myBodyCenterRight0 = CmmAssign
	(CmmGlobal (VanillaReg 1 VNonGcPtr))
	(CmmLit (CmmInt 123 W64))

---------------------------------------------------------------------

myCmmExprOldP :: CmmExpr
myCmmType :: CmmType
CmmLoad myCmmExprOldP myCmmType = myCmlTarget

myCmmExprOldP0 :: CmmExpr
myCmmExprOldP0 = CmmStackSlot Old 8
myCmmType0 :: CmmType
myCmmType0 = gcWord dflags0

myCmmTickish :: CmmTickish
CmmTick myCmmTickish = myBodyCenterLeft

myCmmTickish0 :: CmmTickish
myCmmTickish0 = SourceNote 
	(mkRealSrcSpan
		(mkRealSrcLoc "samples/sample.cmm" 2 1)
		(mkRealSrcLoc "samples/sample.cmm" 4 2))
	"cmm_main"

myCmmReg :: GlobalReg
CmmAssign (CmmGlobal myCmmReg) (CmmLit (CmmInt 123 W64)) = myBodyCenterRight

---------------------------------------------------------------------

myArea :: Area
myCmmStackSlotInt :: Int
CmmStackSlot myArea myCmmStackSlotInt = myCmmExprOldP

---------------------------------------------------------------------

checkBlock :: Block CmmNode e x -> String
checkBlock = \case
	BlockCO _ _ -> "BlockCO"
	BlockCC _ _ _ -> "BlockCC"
	BlockOC _ _ -> "BlockOC"
	BNil -> "BNil"
	BMiddle _ -> "BMiddle"
	BCat _ _ -> "BCat"
	BSnoc _ _ -> "BSnoc"
	BCons _ _ -> "BCons"

checkCmmNode :: CmmNode e x -> String
checkCmmNode = \case
	CmmEntry _ _ -> "CmmEntry"
	CmmComment _ -> "CmmComment"
	CmmTick _ -> "CmmTick"
	CmmUnwind _ -> "CmmUnwind"
	CmmAssign _ _ -> "CmmAssign"
	CmmStore _ _ -> "CmmStore"
	CmmUnsafeForeignCall _ _ _ -> "CmmUnsafeForeignCall"
	CmmBranch _ -> "CmmBranch"
	CmmCondBranch _ _ _ _ -> "CmmCondBranch"
	CmmSwitch _ _ -> "CmmSwitch"
	CmmCall _ _ _ _ _ _ -> "CmmCall"
	CmmForeignCall _ _ _ _ _ _ _ -> "CmmForeignCall"

checkCmmExpr :: CmmExpr -> String
checkCmmExpr = \case
	CmmLit _ -> "CmmLit"
	CmmLoad _ _ -> "CmmLoad"
	CmmReg _ -> "CmmReg"
	CmmMachOp _ _ -> "CmmMachOp"
	CmmStackSlot _ _ -> "CmmStackSlot"
	CmmRegOff _ _ -> "CmmRegOff"
