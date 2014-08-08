#!/bin/sh
cd www
cabal configure --ghcjs
cabal build
