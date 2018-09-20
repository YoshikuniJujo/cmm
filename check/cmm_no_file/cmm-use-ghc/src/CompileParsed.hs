{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CompileParsed (compileSimple, compileCmmGroup) where

import Control.Monad.IO.Class
import System.FilePath

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
import DriverPipeline

import CheckCmmParse
import Tools

import qualified Stream

compileSimple :: IO ()
compileSimple = compileCmmGroup "simple" myCmmParsed0

compileCmmGroup :: FilePath -> CmmGroup -> IO ()
compileCmmGroup nm cmm = doPipeline nm cmm >> linkFoo nm

doPipeline :: FilePath -> CmmGroup -> IO ()
doPipeline nm cmm = liftIO $ do
	us <- mkSplitUniqSupply 'S'
	let	initTopSRT = initUs_ us emptySRT
	(_, cmmgroup) <- cmmPipeline myEnv0 initTopSRT cmm
	rawCmms <- cmmToRawCmm dflags0 $ Stream.yield cmmgroup
	let	mod_name = mkModuleName "Cmm$foo.cmm"
		cmm_mod = mkModule (thisPackage dflags0) mod_name
	_ <- codeOutput dflags0 cmm_mod
		("tmp" </> nm <.> "s")
		no_loc NoStubs [] []
		rawCmms
	return ()
	where
	no_loc = ModLocation {
		ml_hs_file = Just "sample/sample.cmm",
		ml_hi_file = panic "no ml_hl_file",
		ml_obj_file = panic "no ml_obj_file" }


linkFoo :: FilePath -> IO ()
linkFoo nm = linkBinary
	dflags0 { outputFile = Just $ "tmp" </> nm }
	["tmp" </> nm <.> "s", "samples/call_cmm_64.c"]
	[]
