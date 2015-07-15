#!/bin/bash
set -u
set -e

# Edit these to choose which Haskell program and html folder to use
app=cordova-create
www=www-create

cp -R $www/* www/

cd hs
cabal sandbox init
cabal install ./cordova ./$app --ghcjs
mkdir -p ../www/js
cp .cabal-sandbox/bin/$app.jsexe/{rts,lib,out,runmain}.js ../www/js/
