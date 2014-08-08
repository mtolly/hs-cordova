#!/bin/bash
set -u
set -e

cd www
cabal configure --ghcjs
cabal build
rm -f dist/build/*/*.jsexe/all.js
