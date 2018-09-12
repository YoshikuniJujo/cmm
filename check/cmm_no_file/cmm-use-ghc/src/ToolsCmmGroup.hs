{-# LANGUAGE LambdaCase, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ToolsCmmGroup where

import Cmm
import Hoopl.Block

cmmProcOrData :: GenCmmDecl d h g -> String
cmmProcOrData = \case
	CmmProc _ _ _ _ -> "CmmProc"
	CmmData _ _ -> "CmmData"

cmmStaticLitUniStr :: CmmStatic -> String
cmmStaticLitUniStr = \case
	CmmStaticLit _ -> "CmmStaticLit"
	CmmUninitialised _ -> "CmmUninitialised"
	CmmString _ -> "CmmString"

isJustO :: MaybeO a b -> Bool
isJustO = \case JustO _ -> True; NothingO -> False

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
