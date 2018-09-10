{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (
	myDynFlags, dflags0, output, out, getResult, outResult,
	getMyBlockId) where

import System.IO.Unsafe

import DynFlags
import Packages
import SysTools
import Outputable

import Lexer

import UniqSupply
import BlockId

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

dflags0 :: DynFlags
dflags0 = unsafePerformIO myDynFlags

out :: Outputable a => a -> IO ()
out = output dflags0

getResult :: ParseResult a -> Maybe a
getResult = \case POk _ x -> Just x; _ -> Nothing

outResult :: Outputable a => ParseResult a -> IO ()
outResult = out . getResult

runUniqSM :: UniqSM a -> IO a
runUniqSM m = do
	us <- mkSplitUniqSupply 'u'
	return (initUs_ us m)

getMyBlockId :: IO BlockId
getMyBlockId = runUniqSM $ do
	u <- getUniqueM
	return $ mkBlockId u
