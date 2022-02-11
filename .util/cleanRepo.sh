#!/bin/env bash 

# Set command arg to 
#  "binary" to only do bin          folder
#  "test"   to only do tests/output folder
#  "lib"    to only do lib          folder
#  "object" to only do the obj      folder
# Nothing to do both
#
#shopt -s nocasematch
#
#---- Get Shell Scripts path  ------------------------------------------------------------
#
SHELLDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
#
CWD="$(pwd)"
#
cd "$SHELLDIR"
#
#---- Get location of find  --------------------------------------------------------------
#
source ./findfind.sh
#
#
#---- Get NL and BLN variables  ----------------------------------------------------------
#
source ./setNL.sh
#
#
#---- Run Script  ------------------------------------------------------------------------
#
#
T="true"
F="false"

ALL=$T

BIN=$F
OBJ=$F
LIB=$F
EX=$F
Pause=$T

for ARG in "$@" 
do
  case "${ARG:0:1}" in
   b | B )
          BIN=$T
          ALL=$F
           ;;
   l | L )
          LIB=$T
          ALL=$F
           ;;
   o | O )
          OBJ=$T
          ALL=$F
           ;;
   n | N )
          Pause=$F
           ;;
   t | T )
          EX=$T
          ALL=$F
           ;;
   *) ALL=$T;;
  esac
done

#---- Run Script  ------------------------------------------------------------------------
#
if [ $ALL = $T ]; then
                 BIN=$T
                 EX=$T
                 OBJ=$T
                 LIB=$T
fi

#Clean out the bin folder
if [ $BIN = $T ]; then
   echo "${NL}BIN Clean: ${FND} ./bin -not -name 'bin' -not -name '.keep' -delete"
   touch ../bin/tmp.txt
   ${FND} ../bin -not -name 'bin' -not -name '.keep' -delete
fi

#Clean out the object file output
if [ $OBJ = $T ]; then
   echo "${NL}OBJ Clean: ${FND} ./obj -not -name 'obj' -not -name '.keep' -delete"
   touch ../obj/tmp.txt
   ${FND} ../obj -not -name 'obj' -not -name '.keep' -delete
fi

#Clean out the object file output
if [ $LIB = $T ]; then
   echo "${NL}LIB Clean: ${FND} ./lib -not -name 'lib' -not -name '.keep' -delete"
   touch ../lib/tmp.txt
   ${FND} ../lib -not -name 'lib' -not -name '.keep' -delete
fi

#Clean out the example problem output
if [ $EX = $T ]; then
   echo "${NL}Test Clean: ${FND} ./tests/output -not -name 'output' -not -name '.keep' -delete"
   touch ../tests/output/tmp.txt
   ${FND} ../tests/output -not -name 'output' -not -name '.keep' -delete
fi
#
#---- Return to calling folder  ----------------------------------------------------------
#
cd "$CWD"
#
#
#---- Check For "nopause"  ---------------------------------------------------------------
#
if [ $Pause = $T ]
then
   read -p "${NL}Repo Now Clean.${BLN}Press [ENTER] to end script ... "
else
   echo "${BLN}Repo Now Clean.${NL}"
fi
