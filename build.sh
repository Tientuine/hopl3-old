#!/bin/sh
#
# Simple shell script for building one of the model languages from the
# textbook, as translated into Haskell. The default language is LET.
#
# Usage: ./build.sh NAME_OF_LANG_TO_BUILD
###

MODEL_LANG=${1:-LET}

if [ ! -f "${MODEL_LANG}/Scanner.hs" -o "${MODEL_LANG}/LexSpec.x" -nt "${MODEL_LANG}/Scanner.hs" ]
then
    alex  ${MODEL_LANG}/LexSpec.x -o ${MODEL_LANG}/Scanner.hs
fi
if [ ! -f "${MODEL_LANG}/Parser.hs" -o "${MODEL_LANG}/Grammar.y" -nt "${MODEL_LANG}/Parser.hs" ]
then
    happy ${MODEL_LANG}/Grammar.y -o ${MODEL_LANG}/Parser.hs
fi

(ghc -main-is ${MODEL_LANG}.Interp.runInterp ${MODEL_LANG}/Interp.hs) && (ghc ${MODEL_LANG}/Tester.hs) && (ghc ${MODEL_LANG}/Repl.hs)

