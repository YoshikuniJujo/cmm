#!/bin/sh

stack exec brainf-cmm-exe $1
stack ghc -- -no-hs-main ../brainfcmm/call_cmm.c ../brainfcmm/io.s ${1%.*}.cmm -o ${1%.*}
