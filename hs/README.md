Some Haskell files in `src` are built from Ruby `.erb` files.
To rebuild all `.erb` into `.hs`, run `make`.
Then `cabal configure --ghcjs && cabal build` builds the executable and library.
