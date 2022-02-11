#!/bin/env bash 
#
# run as:
#         source ./setNL.sh 
#
# NL  is a variable that holds a single NewLine (LF)
# BLN is a variable that holds two NewLines     (LFLF)
#
#
NL=`echo $'\n.'`
NL=${NL%.}
BLN=${NL}${NL}