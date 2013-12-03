// From common.js

// Listen for the DOM content to be loaded. This event is fired when
// parsing of the page's document has finished.
document.addEventListener('DOMContentLoaded', function() {
  var body = document.body;
  var listenerDiv = document.getElementById('listener');
  listenerDiv.addEventListener('load', moduleDidLoad, true);
  listenerDiv.addEventListener('message', handleMessage, true);
  listenerDiv.addEventListener('crash', handleCrash, true);
})

var naclModule;

function updateStatus(opt_message) {
  if (opt_message) {
    statusText = opt_message;
  }
  var statusField = document.getElementById('statusField');
  if (statusField) {
    statusField.innerHTML = statusText;
  }
}

function moduleDidLoad() {
  naclModule = document.getElementById('nacl_module');
  updateStatus('RUNNING');
  naclModule.postMessage('yo');
}

function handleMessage(message) {
  var logEl = document.getElementById('log');
  logEl.textContent += message.data;
}

function handleCrash(event) {
  if (naclModule.exitStatus == -1) {
    updateStatus('CRASHED');
  } else {
    updateStatus('EXITED [' + naclModule.exitStatus + ']');
  }
}
