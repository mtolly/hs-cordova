#!/bin/bash
set -u
set -e

app=hs-cordova-create

cd hs
cabal configure --ghcjs
cabal build
mkdir -p ../www/js
cp dist/build/$app/$app.jsexe/{lib,rts,lib1,out}.js ../www/js/
