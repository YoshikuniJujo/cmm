{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ToolsCmmGroup where

import Cmm

cmmProcOrData :: GenCmmDecl d h g -> String
cmmProcOrData = \case
	CmmProc _ _ _ _ -> "CmmProc"
	CmmData _ _ -> "CmmData"

cmmStaticLitUniStr :: CmmStatic -> String
cmmStaticLitUniStr = \case
	CmmStaticLit _ -> "CmmStaticLit"
	CmmUninitialised _ -> "CmmUninitialised"
	CmmString _ -> "CmmString"
