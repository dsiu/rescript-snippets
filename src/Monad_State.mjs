// Generated by ReScript, PLEASE EDIT WITH CARE

import * as REFP__Reader from "../re-fp/src/REFP__Reader.mjs";

function rng_0(s) {
  var rand = Math.random() - 0.5;
  return [
          s + rand,
          rand
        ];
}

function rbg_0(s) {
  var match = rng_0(s);
  return [
          match[0],
          match[1] > 0.0
        ];
}

console.log(rng_0(3.0));

console.log(rbg_0(2.0));

var match = rng_0(2.0);

var r0 = match[1];

var s1 = match[0];

var match$1 = rng_0(s1);

var r1 = match$1[1];

var s2 = match$1[0];

var match$2 = rng_0(s2);

var r2 = match$2[1];

console.log(r0 + r1 + r2);

function rng(s) {
  var rand = Math.random() - 0.5;
  return /* State */{
          _0: s + rand,
          _1: rand
        };
}

function rbg(s) {
  var match = rng(s);
  return /* State */{
          _0: match._0,
          _1: match._1 > 0.0
        };
}

var nextLong = rng;

var nextBool = rbg;

console.log(rng(1.0));

console.log(rng(2.0));

console.log(rng(3.0));

console.log(rbg(1.0));

console.log(rbg(2.0));

console.log(rbg(3.0));

console.log("Use re-fp");

function getSeed(param) {
  return REFP__Reader.from(0.14, param);
}

function $$double(n) {
  return n * 2.0;
}

function addOne(n) {
  return n + 1.0;
}

function f(param) {
  return REFP__Reader.map((function (param) {
                return REFP__Reader.map((function (param) {
                              return REFP__Reader.map(getSeed, $$double, param);
                            }), addOne, param);
              }), addOne, param);
}

console.log(f(0));

var s0 = 2.0;

var R;

export {
  rng_0 ,
  rbg_0 ,
  s0 ,
  s1 ,
  r0 ,
  s2 ,
  r1 ,
  r2 ,
  rng ,
  rbg ,
  nextLong ,
  nextBool ,
  R ,
  getSeed ,
  $$double ,
  addOne ,
  f ,
  
}
/*  Not a pure module */
