#!/bin/sh

stack exec brainf-cmm $1
stack ghc -- -no-hs-main call_cmm.c io.s ${1%.*}.cmm -o ${1%.*}
