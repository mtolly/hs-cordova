Some Haskell files in `src` are built from Ruby `.erb` files.
To rebuild all `.erb` into `.hs`, run `make`.
Requires at least Ruby 2.0.
Then `cabal configure --ghcjs && cabal build` builds the executable and library.
