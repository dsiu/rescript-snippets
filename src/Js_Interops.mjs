// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";

var isBefore = ((a, b) => a < b);

setTimeout((function () {
        console.log("Hello");
      }), 2000);

var devOpt = typeof __DEV__ === "undefined" ? undefined : __DEV__;

if (devOpt !== undefined) {
  console.log("development mode");
} else {
  console.log("production mode");
}

var someNumber = Math.random();

console.log(window.location.ancestorOrigins.length);

function f(x) {
  x.hi(true, 2);
}

var devOpt$1 = devOpt === undefined ? undefined : Caml_option.some(devOpt);

export {
  isBefore ,
  devOpt$1 as devOpt,
  someNumber ,
  f ,
}
/*  Not a pure module */
