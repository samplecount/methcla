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

// function getMousePos(canvas, evt) {
//   var rect = canvas.getBoundingClientRect();
//   return {
//     x: evt.clientX - rect.left,
//     y: evt.clientY - rect.top
//   };
// }
// 
// var canvas = document.getElementById('thaddeus-canvas');
// 
// canvas.onmousedown = function(evt) {
//   var pos = getMousePos(canvas, evt);
//   naclModule.postMessage('startVoice:' + getFrequencyElement().value);
//   
//     mouseIsDown = true;
// }
// 
// canvas.onmouseup = function(e){
//     if(mouseIsDown) mouseClick(e);
// 
//     mouseIsDown = false;
// }
// 
// canvas.onmousemove = function(e){
//     if(!mouseIsDown) return;
// 
//     mainLayer.trans.x = e.x - dragOffset.x;
//     mainLayer.trans.y = e.y - dragOffset.y;
//     return false;
// }
// 
// function mouseClick(e){
//     // click action
// }
// canvas.addEventListener('mousemove', function(evt) {
//   var mousePos = getMousePos(canvas, evt);
//   var message = 'Mouse position: ' + mousePos.x + ',' + mousePos.y;
//   writeMessage(canvas, message);
// }, false);
// 
