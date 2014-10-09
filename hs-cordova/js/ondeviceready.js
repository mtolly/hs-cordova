var hs_deviceReadyQueue = [];

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
