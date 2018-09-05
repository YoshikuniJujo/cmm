{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import DriverPipeline
import HscMain
import DynFlags
import CmmParse
import CmmPipeline
import CmmBuildInfoTables
import CmmInfo
import UniqSupply
import Outputable
import qualified Stream
import HscTypes
import Module
import CodeOutput

import Tools

main :: IO ()
main = do
	dflags <- myDynFlags
	env <- newHscEnv dflags
	hscCompileCmmFile env "../manual/sample.cmm" "tmp/sample.s"
	linkBinary dflags ["tmp/sample.s", "../manual/call_cmm.c"] []

	((wm, em), Just cmm) <- parseCmmFile dflags "../manual/sample.cmm"
	output dflags (show <$> wm, show <$> em)
	output dflags cmm
	putStrLn ""
	us <- mkSplitUniqSupply 'S'
	let initTopSRT = initUs_ us emptySRT
	(_, cmmgroup) <- cmmPipeline env initTopSRT cmm
	output dflags cmmgroup
	putStrLn ""
	rawCmms <- cmmToRawCmm dflags $ Stream.yield cmmgroup
	output dflags =<< Stream.collect rawCmms
	let	mod_name = mkModuleName $ "Cmm$" ++ "sample.cmm"
		cmm_mod = mkModule (thisPackage dflags) mod_name
	_ <- codeOutput dflags cmm_mod "tmp/sample2.s" no_loc NoStubs [] []
		rawCmms
	return ()
	where
	no_loc = ModLocation {
		ml_hs_file = Just "sample.cmm",
		ml_hi_file = panic "boo",
		ml_obj_file = panic "boo" }
