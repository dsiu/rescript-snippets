// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";

function log(prim) {
  console.log(prim);
}

function add(n, x) {
  return x + n | 0;
}

function times(n, x) {
  return Math.imul(x, n);
}

function add1Times2(param) {
  return Stdlib__Function.compose((function (param) {
                return param + 1 | 0;
              }), (function (param) {
                return (param << 1);
              }), param);
}

function add5Times3(param) {
  return Stdlib__Function.compose((function (param) {
                return param + 5 | 0;
              }), (function (param) {
                return Math.imul(param, 3);
              }), param);
}

var prim = add1Times2(1);

console.log(prim);

var prim$1 = add5Times3(2);

console.log(prim$1);

function twice(f) {
  return function (param) {
    return Stdlib__Function.compose(f, f, param);
  };
}

function add1(param) {
  return param + 1 | 0;
}

function add1Twice(param) {
  return Stdlib__Function.compose(add1, add1, param);
}

var prim$2 = Stdlib__Function.compose(add1, add1, 9);

console.log(prim$2);

function add1ThenMultiply(param) {
  return Stdlib__Function.compose(add1, times, param);
}

var prim$3 = Curry._1(Stdlib__Function.compose(add1, times, 2), 7);

console.log(prim$3);

function f(x, y, z) {
  return x + Math.imul(y, z) | 0;
}

function g(param) {
  return param[0] + Math.imul(param[1], param[2]) | 0;
}

console.log("f");

console.log(7);

console.log("g");

var prim$4 = g([
      1,
      2,
      3
    ]);

console.log(prim$4);

function h(x, y, z) {
  
}

function h1(param, param$1) {
  return h(1, param, param$1);
}

function h2(param) {
  return h(1, true, param);
}

var h3;

var compose = Stdlib__Function.compose;

export {
  log ,
  compose ,
  add ,
  times ,
  add1Times2 ,
  add5Times3 ,
  twice ,
  add1 ,
  add1Twice ,
  add1ThenMultiply ,
  f ,
  g ,
  h ,
  h1 ,
  h2 ,
  h3 ,
}
/* prim Not a pure module */
