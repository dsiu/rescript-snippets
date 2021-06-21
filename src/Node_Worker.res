
// binding a node global value
// Small note, ReScript supports the equivalent __FILE__ macro directly, so no need for a binding.
module Global = {
  @val
  external __filename: string = "__filename"
}


module WorkerThreads = {
  module Worker = {
    type t
    type sendCallback = string => unit

    // JS: worker.on
    @send
    external on: (t, string, sendCallback) => unit = "on"
  }

  module ParentPort = {
    type t

    // JS: parentPort.postMessage
    @send
    external postMessage: (t, string) => unit = "postMessage"
  }


  // const { Worker, isMainThread, parentPort } = require("worker_threads");
  @module("worker_threads") @value
  external isMainThread: bool = "isMainThread"

  @module("worker_threads") @value
  external parentPort: ParentPort.t = "parentPort"

  @module("worker_threads") @new
  external makeWorker: string => Worker.t = "Worker"
}

if WorkerThreads.isMainThread {
  //  let worker = WorkerThreads.makeWorker(Global.__filename)
  let worker = WorkerThreads.makeWorker(__FILE__)
  worker->WorkerThreads.Worker.on("message", msg => {
    Js.log(msg)
  })
} else {
  WorkerThreads.parentPort->WorkerThreads.ParentPort.postMessage("Hello world!")
}
