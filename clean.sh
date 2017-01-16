#!/bin/sh
#
# Simple shell script for building one of the model languages from the
# textbook, as translated into Haskell. The default language is LET.
#
# Usage: ./build.sh NAME_OF_LANG_TO_BUILD
###

MODEL_LANG=${1:-LET}

rm -f ${MODEL_LANG}/Interp ${MODEL_LANG}/Tester ${MODEL_LANG}/Repl
rm -f ${MODEL_LANG}/*.hi ${MODEL_LANG}/*.o
rm -f ${MODEL_LANG}/Scanner.hs ${MODEL_LANG}/Parser.hs

