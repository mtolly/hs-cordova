#!/bin/bash
set -u
set -e

# Edit these to choose which Haskell program and html folder to use
app=hs-cordova-create
www=www-create

cp -R $www/* www/

cd hs
make # feel free to delete this if you haven't edited the Ruby files
cabal configure --ghcjs
cabal build
mkdir -p ../www/js
cp dist/build/$app/$app.jsexe/{rts,lib,out,runmain}.js ../www/js/
