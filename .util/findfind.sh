#!/bin/env bash 
#
#---- Find the location of find  ---------------------------------------------------------
#
# Note bash on windows sometimes calls C:\Windows\System32\find.exe, 
#   but want Bash "find" with no extension
#
FND="find"
for FBIN in `which -a find`; do
   case "$FBIN" in
       *Win* | *WIN* | *win* ) continue ;;
       * ) 
          FND="$FBIN"
          break
          ;;
   esac
done
#
#---- End Script  ------------------------------------------------------------------------
#
