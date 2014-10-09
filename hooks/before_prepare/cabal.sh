#!/bin/bash
set -u
set -e

cd hs
make
cabal clean
cabal configure --ghcjs
cabal build
mkdir -p ../www/js
cp dist/build/*/*.jsexe/{lib,rts,lib1,out}.js ../www/js/
