// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Worker_threads from "worker_threads";

var Global = {};

var $$Worker = {};

var ParentPort = {};

var WorkerThreads = {
  $$Worker: $$Worker,
  ParentPort: ParentPort
};

if (Worker_threads.isMainThread) {
  var worker = new Worker_threads.Worker("Node_Worker.res");
  worker.on("message", (function (msg) {
          console.log(msg);
        }));
} else {
  Worker_threads.parentPort.postMessage("Hello world!");
}

export {
  Global ,
  WorkerThreads ,
}
/*  Not a pure module */
