{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CompileParsed where

import Control.Monad.IO.Class

import Cmm
import CmmPipeline
import CmmBuildInfoTables
import CmmInfo
import UniqSupply
import Module
import DynFlags
import CodeOutput
import Panic
import HscTypes

import CheckCmmParse
import Tools

import qualified Stream

doPipeline :: IO ()
doPipeline = liftIO $ do
	us <- mkSplitUniqSupply 'S'
	let	initTopSRT = initUs_ us emptySRT
	(_, cmmgroup) <- cmmPipeline myEnv0 initTopSRT myCmmParsed
	rawCmms <- cmmToRawCmm dflags0 $ Stream.yield cmmgroup
	let	mod_name = mkModuleName "Cmm$foo.cmm"
		cmm_mod = mkModule (thisPackage dflags0) mod_name
	_ <- codeOutput dflags0 cmm_mod "tmp/foo.s" no_loc NoStubs [] [] rawCmms
	return ()
	where
	no_loc = ModLocation {
		ml_hs_file = Just "sample/sample.cmm",
		ml_hi_file = panic "no ml_hl_file",
		ml_obj_file = panic "no ml_obj_file" }
