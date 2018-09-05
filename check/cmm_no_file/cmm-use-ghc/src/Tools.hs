{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (myDynFlags, output) where

import DynFlags
import Packages
import SysTools
import Outputable

myTopDir :: FilePath
myTopDir = "topdir"
-- myTopDir = "/home/tatsuya/.stack/" ++
--	"programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/"

myDynFlags :: IO DynFlags
myDynFlags = do
	mySettings <- initSysTools $ Just myTopDir
	myLlvmTargets <- initLlvmTargets $ Just myTopDir
	dflags <- initDynFlags $ defaultDynFlags mySettings myLlvmTargets
	(dflags', _ ) <- initPackages dflags
	let	dflags'' = gopt_set dflags' Opt_NoHsMain
		dflags''' = dflags'' { outputFile = Just "tmp/sample" }
	return dflags'''

output :: Outputable a => DynFlags -> a -> IO ()
output df = putStrLn . showSDoc df . ppr
