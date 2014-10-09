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
