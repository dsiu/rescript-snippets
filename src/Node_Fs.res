// https://kevanstannard.github.io/rescript-blog/node-check-is-file.html
//

module Fs = {
  module Stats = {
    type t
    @send external isDirectory: t => bool = "isDirectory"
    @send external isFile: t => bool = "isFile"
  }

  @module("fs") external statSync: string => Stats.t = "statSync"
}

let isFile = (path: string): bool => {
  let stats = Fs.statSync(path)
  Fs.Stats.isFile(stats)
}
