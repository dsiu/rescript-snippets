// Generated by ReScript, PLEASE EDIT WITH CARE

import * as SafeReadFile from "./safeReadFile";

function safeReadFile(prim) {
  return SafeReadFile(prim);
}

SafeReadFile("Result.res").then(function (x) {
      return Promise.resolve((console.log(x !== undefined ? x : "No cache, fetch data."), undefined));
    });

export {
  safeReadFile ,
}
/*  Not a pure module */
