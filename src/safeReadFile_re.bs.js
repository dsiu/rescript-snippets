// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var SafeReadFile = require("./safeReadFile");

function safeReadFile(prim) {
  return SafeReadFile(prim);
}

SafeReadFile("Result.res").then(function (x) {
      return Promise.resolve((console.log(x !== undefined ? x : "No cache, fetch data."), undefined));
    });

exports.safeReadFile = safeReadFile;
/*  Not a pure module */