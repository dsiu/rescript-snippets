// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";

function square(x) {
  var result = Math.imul(x, x);
  return {
          result: result,
          logs: ["Squared " + x + " to get " + result]
        };
}

function addOne(x) {
  var result = x + 1 | 0;
  return {
          result: result,
          logs: ["Added 1 to " + x + " to get to " + result]
        };
}

function wrapWithLog(x) {
  return {
          result: x,
          logs: []
        };
}

function runWithLogs(input, transform) {
  var resultWithLogs = Curry._1(transform, input.result);
  return {
          result: resultWithLogs.result,
          logs: Belt_Array.concat(input.logs, resultWithLogs.logs)
        };
}

var a_logs = [];

var a = {
  result: 5,
  logs: a_logs
};

var b = runWithLogs(a, addOne);

console.log(runWithLogs(b, square));

var peter = "Peter";

function isABoy(x) {
  return x.concat(" is a boy");
}

console.log(Belt_Option.flatMap(peter, isABoy));

console.log(Belt_Option.flatMap(undefined, isABoy));

var noone;

export {
  square ,
  addOne ,
  wrapWithLog ,
  runWithLogs ,
  a ,
  b ,
  peter ,
  noone ,
  isABoy ,
  
}
/* b Not a pure module */
