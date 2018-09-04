{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import DriverPipeline
import HscMain
import DynFlags
import Packages
import SysTools
import CmmParse
import Outputable

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
	(dflags', _) <- initPackages dflags
	let	dflags'' = gopt_set dflags' Opt_NoHsMain
		dflags''' = dflags'' { outputFile = Just "tmp/sample" }
	linkBinary dflags''' ["tmp/sample.s", "../manual/call_cmm.c"] []
	((wm, em), mcg) <- parseCmmFile dflags''' "../manual/sample.cmm"
	output dflags (show <$> wm, show <$> em)
	output dflags''' mcg

output :: Outputable a => DynFlags -> a -> IO ()
output df = putStrLn . showSDoc df . ppr
