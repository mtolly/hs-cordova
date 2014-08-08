This is the basic [Cordova] example project (what you get after running `cordova create`), rewritten in Haskell using [GHCJS].

  [Cordova]: http://cordova.apache.org/
  [GHCJS]: https://github.com/ghcjs/ghcjs

Things to note:

  * A hook is provided so running `cordova build` or `cordova run` automatically configures & builds the Haskell project.

  * You can't perform `document.addEventListener('deviceready', ...)` from the Haskell code,
    so instead, `index.html` does it, and the listener simply starts execution of Haskell's `main`.
    The reason for this is because the GHCJS runtime executes the Haskell main function "in the background",
    so you can't rely on anything being done before the DOM/Cordova/anything is done loading.
