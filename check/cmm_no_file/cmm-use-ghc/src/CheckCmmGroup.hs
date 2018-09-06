{-# LANGUAGE OverloadedStrings #-}
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
import Hoopl.Graph
import Hoopl.Block
import Hoopl.Label
import BlockId
import Unique

import Tools

helloL :: CLabel
helloL = mkCmmCodeLabel (thisPackage dflags0) "hello"

sample1 :: GenCmmGroup CmmStatics CmmTopInfo CmmGraph
sample1 = [
	CmmProc myTopInfo helloL [] cmmGraph1,
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

entry1 :: CmmNode C O
entry1 = CmmEntry (uniqueToLbl 123) GlobalScope

branch1 :: CmmNode O C
branch1 = CmmBranch (uniqueToLbl 321)

block1 :: Block CmmNode C C
block1 = BlockCC
	entry1
	(BMiddle $ CmmAssign (CmmGlobal Sp) (CmmLit $ CmmInt 654 W8))
	branch1

graph1 :: Graph CmmNode C C
graph1 = GMany NothingO (mapSingleton (uniqueToLbl 456) block1) NothingO

cmmGraph1 :: CmmGraph
cmmGraph1 = CmmGraph (mkBlockId $ getUnique (456 :: Int)) graph1
