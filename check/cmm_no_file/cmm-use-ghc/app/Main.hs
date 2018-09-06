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
	env <- newHscEnv dflags0
	hscCompileCmmFile env "../manual/sample.cmm" "tmp/sample.s"
	linkBinary dflags0 ["tmp/sample.s", "../manual/call_cmm.c"] []

	((wm, em), Just cmm) <- parseCmmFile dflags0 "../manual/sample.cmm"
	output dflags0 (show <$> wm, show <$> em)
	output dflags0 cmm
	putStrLn ""
	us <- mkSplitUniqSupply 'S'
	let initTopSRT = initUs_ us emptySRT
	(_, cmmgroup) <- cmmPipeline env initTopSRT cmm
	output dflags0 cmmgroup
	putStrLn ""
	rawCmms <- cmmToRawCmm dflags0 $ Stream.yield cmmgroup
	output dflags0 =<< Stream.collect rawCmms
	let	mod_name = mkModuleName $ "Cmm$" ++ "sample.cmm"
		cmm_mod = mkModule (thisPackage dflags0) mod_name
	_ <- codeOutput dflags0 cmm_mod "tmp/sample2.s" no_loc NoStubs [] []
		rawCmms
	return ()
	where
	no_loc = ModLocation {
		ml_hs_file = Just "sample.cmm",
		ml_hi_file = panic "boo",
		ml_obj_file = panic "boo" }
