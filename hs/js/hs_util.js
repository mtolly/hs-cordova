// "deviceready" handler

var hs_deviceReadyQueue = [];
// An array if device is not ready yet, null if it is ready.

document.addEventListener('deviceready', function() {
  for (var i = 0; i < hs_deviceReadyQueue.length; i++) {
    setTimeout(hs_deviceReadyQueue[i], 0);
  }
  hs_deviceReadyQueue = null;
})

function hs_onDeviceReady(callback) {
  if (hs_deviceReadyQueue === null) {
    setTimeout(callback, 0);
  }
  else {
    hs_deviceReadyQueue.push(callback);
  }
}

// Error handling

function hs_good(callback) {
  return function(result){ callback([0, result]); };
}

function hs_error(callback) {
  return function(result){ callback([1, result]); }
}

// Equality for enum macros

// Compares using ===, or for objects, deep equality.
// Modified from http://designpepper.com/blog/drips/object-equality-in-javascript.html
function hs_deep_equal(a, b) {
    if (typeof a !== 'object' || typeof b !== 'object') return a === b;

    // Create arrays of property names
    var aProps = Object.getOwnPropertyNames(a);
    var bProps = Object.getOwnPropertyNames(b);

    // If number of properties is different,
    // objects are not equivalent
    if (aProps.length != bProps.length) {
        return false;
    }

    for (var i = 0; i < aProps.length; i++) {
        var propName = aProps[i];

        // If values of same property are not equal,
        // objects are not equivalent
        if (!hs_deep_equal(a[propName], b[propName])) {
            return false;
        }
    }

    // If we made it this far, objects
    // are considered equivalent
    return true;
}
