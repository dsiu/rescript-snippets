// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");

function map_pair(r, param) {
  return [
          Curry._1(r, param[0]),
          Curry._1(r, param[1])
        ];
}

var array = [
  3,
  "a"
];

var x = {
  f: "foo"
};

exports.x = x;
exports.map_pair = map_pair;
exports.array = array;
/* No side effect */
