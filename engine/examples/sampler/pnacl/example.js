// Copyright 2012-2013 Samplecount S.L.
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var currentRequest;

function request(msg, cont) {
  if (currentRequest != null) {
    throw new Error('Request in progress');
  } else {
    currentRequest = function (response) {
      if (response.hasOwnProperty('error') && response['error']) {
        throw new Error(response['error_message'] || 'Unknown error');
      } else {
        cont(response);
      }
    }
    common.naclModule.postMessage(msg);
  }
}

function handleMessage(event) {
  if (currentRequest != null) {
    var func = currentRequest;
    currentRequest = null;
    func(event.data);
  }
}

function moduleDidLoad() {
  common.hideModule();
  request(
    { type: 'copyFiles' },
    function (msg) {
      common.logMessage('Request ' + msg.type + ' finished');
      request( { type: 'createEngine' },
        function (msg) {
          common.logMessage('Request ' + msg.type + ' finished');
        }
      );
    }
  );
}

function playSound(id) {
  common.naclModule.postMessage(
    { type: 'startVoice'
    , id: 0
    , sound: id
    , amp: 1
    , rate: 1 }
  );
}

// Called by the common.js module.
function attachListeners() {
  document.getElementById('playSound0').addEventListener('click', function () { playSound(0) });
  document.getElementById('playSound1').addEventListener('click', function () { playSound(1) });
  document.getElementById('playSound2').addEventListener('click', function () { playSound(2) });
  // document.getElementById('frequencyField').addEventListener('change',
  //     changeFrequency);
}

// Called by the common.js module.
function domContentLoaded(name, tc, config, width, height, attrs) {
  navigator.webkitPersistentStorage.requestQuota(3268608,
    function(bytes) {
      common.updateStatus(
          'Allocated ' + bytes + ' bytes of storage.');
      common.attachDefaultListeners();
      common.createNaClModule(name, tc, config, width, height);
    },
    function(e) { alert('Failed to allocate space') }
  );
}
