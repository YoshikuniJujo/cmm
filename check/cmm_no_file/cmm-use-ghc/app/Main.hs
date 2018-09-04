{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import HscMain
import DynFlags
import SysTools

myTopDir :: FilePath
myTopDir = "topdir"
-- myTopDir = "/home/tatsuya/.stack/" ++
--	"programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/"

main :: IO ()
main = do
	mySettings <- initSysTools $ Just myTopDir
	myLlvmTargets <- initLlvmTargets $ Just myTopDir
	dflags <- initDynFlags $ defaultDynFlags mySettings myLlvmTargets
	env <- newHscEnv dflags
	hscCompileCmmFile env "../manual/sample.cmm" "tmp/sample.s"
