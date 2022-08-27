// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";

function log(prim) {
  console.log(prim);
}

function add42(x) {
  return x + 42 | 0;
}

console.log(43);

console.log(45);

var prim = Belt_List.map({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, add42);

console.log(prim);

function twoIsLessThan(x) {
  return x > 2;
}

console.log(false);

console.log(true);

var prim$1 = Belt_List.map({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, twoIsLessThan);

console.log(prim$1);

function adderWithPluggableLogger(logger, x, y) {
  Curry._2(logger, "x", x);
  Curry._2(logger, "y", y);
  var result = x + y | 0;
  Curry._2(logger, "x+y", result);
  return result;
}

function consoleLogger(argName, argValue) {
  console.log("" + argName + "=" + String(argValue) + "");
}

function addWithConsoleLogger(param, param$1) {
  return adderWithPluggableLogger(consoleLogger, param, param$1);
}

adderWithPluggableLogger(consoleLogger, 1, 2);

adderWithPluggableLogger(consoleLogger, 42, 99);

function redLogger(argName, argValue) {
  var message = "" + argName + "=" + String(argValue) + "";
  console.log("[red]" + message);
}

function addWithRedLogger(param, param$1) {
  return adderWithPluggableLogger(redLogger, param, param$1);
}

adderWithPluggableLogger(redLogger, 1, 2);

adderWithPluggableLogger(redLogger, 42, 99);

export {
  log ,
  add42 ,
  twoIsLessThan ,
  adderWithPluggableLogger ,
  consoleLogger ,
  addWithConsoleLogger ,
  redLogger ,
  addWithRedLogger ,
}
/*  Not a pure module */
