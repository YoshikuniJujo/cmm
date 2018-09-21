{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseBfSample1 where

import System.IO.Unsafe

import DynFlags
import Cmm
import CmmParse
import CLabel

import Tools
import ToolsCmmGroup

myBfSample1 :: CmmGroup
myBfSample1 = unsafePerformIO $ do
	(_, Just cmm) <- parseCmmFile dflags0 "samples/bfSample1.cmm"
	return cmm

myCmmData0 :: GenCmmDecl CmmStatics CmmTopInfo CmmGraph
myCmmData0 = CmmData
	(Section Data mkCmmDataLabelTPDFMem0)
	(Statics mkCmmDataLabelTPDFMem0 [cmmUninitialised30000_0])

----------------------------------------------------------------------

[
	CmmData	(Section Data mkCmmDataLabelTPDFMem1)
		(Statics mkCmmDataLabelTPDFMem2 [cmmUninitialised30000]),
	c ] = myBfSample1

mkCmmDataLabelTPDFMem0 :: CLabel
mkCmmDataLabelTPDFMem0 = mkCmmDataLabel (thisPackage dflags0) "memory"
cmmUninitialised30000_0 :: CmmStatic
cmmUninitialised30000_0 = CmmUninitialised 30000
