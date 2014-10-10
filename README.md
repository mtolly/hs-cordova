This is the basic [Cordova] example project (what you get after running `cordova create`),
rewritten in Haskell using [GHCJS].

  [Cordova]: http://cordova.apache.org/
  [GHCJS]: https://github.com/ghcjs/ghcjs

Things to note:

  * A hook is provided so running `cordova build` or `cordova run` automatically configures & builds the Haskell project.

  * You can't perform `document.addEventListener('deviceready', ...)` directly from the Haskell code,
    so the `hs-cordova` library includes extra JS code which installs the listener.
    The reason for this is because the GHCJS runtime executes the Haskell main function "in the background",
    so you can't rely on anything being done before the DOM/Cordova is done loading.

Also in development is a library of bindings to the Cordova plugin APIs.
Completed functionality:

  * Waiting for "deviceready"

  * [Camera](http://plugins.cordova.io/#/package/org.apache.cordova.camera)

  * [Device](http://plugins.cordova.io/#/package/org.apache.cordova.device)

  * [Vibration](http://plugins.cordova.io/#/package/org.apache.cordova.vibration)
