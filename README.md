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
  * [`org.apache.cordova.camera`](http://plugins.cordova.io/#/package/org.apache.cordova.camera)
  * [`org.apache.cordova.device`](http://plugins.cordova.io/#/package/org.apache.cordova.device)
  * [`org.apache.cordova.vibration`](http://plugins.cordova.io/#/package/org.apache.cordova.vibration)
  * [`org.apache.cordova.dialogs`](http://plugins.cordova.io/#/package/org.apache.cordova.dialogs)
  * [`org.apache.cordova.network-information`](http://plugins.cordova.io/#/package/org.apache.cordova.network-information)
  * [`org.apache.cordova.geolocation`](http://plugins.cordova.io/#/package/org.apache.cordova.geolocation)
  * [`org.apache.cordova.statusbar`](http://plugins.cordova.io/#/package/org.apache.cordova.statusbar)
  * [`org.apache.cordova.battery-status`](http://plugins.cordova.io/#/package/org.apache.cordova.battery-status)
  * [`org.apache.cordova.device-orientation`](http://plugins.cordova.io/#/package/org.apache.cordova.device-orientation)
  * [`org.apache.cordova.device-motion`](http://plugins.cordova.io/#/package/org.apache.cordova.device-motion)
  * [`org.apache.cordova.file`](http://plugins.cordova.io/#/package/org.apache.cordova.file) (needs testing)

Tips for using the bindings:

  * Asynchronous JS functions simply return their value synchronously in Haskell.

  * Double-callback JS functions, where you supply "success" and "error" callbacks,
    return "Either error success" in Haskell.

  * Functions that install event handlers take the following form in Haskell:

        arguments -> IO (IO ())

    The `IO ()` you get back is the "deregister" function,
    which removes the event handler.

  * JS enumeration-like values become real Haskell `Enum` types.

  * Options arguments passed to JS functions become Haskell records,
    with a `Data.Default.Default` instance.
    Use the `def` value as an empty options object,
    or use `def { someField = ... }` to just pass specific fields.
