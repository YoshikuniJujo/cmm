{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ParseCmmMemory (myCmmMemory, myCmmMemory0) where

import System.IO.Unsafe

import DynFlags
import Cmm
import CmmParse
import CLabel

import Tools
import ToolsCmmGroup

myCmmMemory :: CmmGroup
myCmmMemory = unsafePerformIO $ do
	(_, Just cmm) <- parseCmmFile dflags0 "samples/memory.cmm"
	return cmm

myCmmMemory0 :: CmmGroup
myCmmMemory0 = [CmmData mySection0 myCmmStatics0]

----------------------------------------------------------------------

mySection :: Section
myCmmStatics :: CmmStatics
[CmmData mySection myCmmStatics] = myCmmMemory

mySection0 :: Section
mySection0 = Section mySectionType0 myCLabel0
myCmmStatics0 :: CmmStatics
myCmmStatics0 = Statics myStaticsCLabel0 [myCmmStatic0]

----------------------------------------------------------------------

mySectionType :: SectionType
myCLabel :: CLabel
Section mySectionType myCLabel = mySection

mySectionType0 :: SectionType
mySectionType0 = Data
myCLabel0 :: CLabel
myCLabel0 = mkCmmDataLabel (thisPackage dflags0) "memory"

myStaticsCLabel :: CLabel
myCmmStatic :: CmmStatic
Statics myStaticsCLabel [myCmmStatic] = myCmmStatics

myStaticsCLabel0 :: CLabel
myStaticsCLabel0 = myCLabel0
myCmmStatic0 :: CmmStatic
myCmmStatic0 = CmmUninitialised 30000
