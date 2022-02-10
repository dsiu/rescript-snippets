// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

function dup(x) {
  return [
          x,
          x
        ];
}

var src = [
  2,
  4,
  6
];

function flatMap(m, f) {
  return Belt_Array.reduce(m, [], (function (a, x) {
                return Belt_Array.concat(a, Curry._1(f, x));
              }));
}

console.log(Belt_Array.map(src, dup));

console.log(flatMap(src, dup));

function dupl(x) {
  return {
          hd: x,
          tl: {
            hd: x,
            tl: /* [] */0
          }
        };
}

var srcl = {
  hd: 2,
  tl: {
    hd: 4,
    tl: {
      hd: 6,
      tl: /* [] */0
    }
  }
};

function flatMapL(m, f) {
  return Belt_List.reduce(m, /* [] */0, (function (a, x) {
                return Belt_List.concat(a, Curry._1(f, x));
              }));
}

console.log(Belt_List.toArray(Belt_List.map(srcl, dupl)));

console.log(Belt_List.toArray(flatMapL(srcl, dupl)));

exports.dup = dup;
exports.src = src;
exports.flatMap = flatMap;
exports.dupl = dupl;
exports.srcl = srcl;
exports.flatMapL = flatMapL;
/*  Not a pure module */