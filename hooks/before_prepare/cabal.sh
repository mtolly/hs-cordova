#!/bin/bash
set -u
set -e

app=hs-cordova-create

cd hs
cabal configure --ghcjs
node dist/setup/setup.jsexe/all.js build
# ^ "cabal build" has issue with Custom cabal setup, see
# https://github.com/ghcjs/ghcjs/issues/262
mkdir -p ../www/js
cp dist/build/$app/$app.jsexe/{lib,rts,lib1,out}.js ../www/js/
