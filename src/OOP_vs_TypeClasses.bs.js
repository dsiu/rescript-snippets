// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";

function log(prim) {
  console.log(prim);
  
}

function coord(p) {
  return [
          p._0,
          p._1
        ];
}

var prim = coord({
      TAG: /* FloatPoint */0,
      _0: 1.0,
      _1: 2.0
    });

console.log(prim);

var prim$1 = coord({
      TAG: /* IntPoint */1,
      _0: 3,
      _1: 4
    });

console.log(prim$1);

function inc(a) {
  a.contents = a.contents + 1 | 0;
  
}

function read(a) {
  return a.contents;
}

function proc(inc, read, x) {
  Curry._1(inc, x);
  Curry._1(inc, x);
  Curry._1(inc, x);
  var prim = Curry._1(read, x);
  console.log(prim);
  
}

proc(inc, read, {
      contents: 5
    });

export {
  log ,
  coord ,
  inc ,
  read ,
  proc ,
  
}
/* prim Not a pure module */
