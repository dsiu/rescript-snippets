// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Caml_int32 = require("rescript/lib/js/caml_int32.js");

function log(prim) {
  console.log(prim);
  
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
  
}

function plus_opt(x, y) {
  if (x !== undefined && y !== undefined) {
    return x + y | 0;
  }
  
}

function minus_opt(x, y) {
  if (x !== undefined && y !== undefined) {
    return x - y | 0;
  }
  
}

function mult_opt(x, y) {
  if (x !== undefined && y !== undefined) {
    return x - y | 0;
  }
  
}

function div_opt(x, y) {
  if (x !== undefined && y !== undefined && y !== 0) {
    return Caml_int32.div(x, y);
  }
  
}

function propagate_none_try(op, x, y) {
  if (x !== undefined && y !== undefined) {
    return Curry._2(op, x, y);
  }
  
}

function propagate_none(op, x, y) {
  if (x !== undefined && y !== undefined) {
    return Curry._2(op, x, y);
  }
  
}

function wrap_output(op, x, y) {
  return Curry._2(op, x, y);
}

function $plus(param, param$1) {
  return propagate_none((function (param, param$1) {
                return param + param$1 | 0;
              }), param, param$1);
}

function $neg(param, param$1) {
  return propagate_none((function (param, param$1) {
                return param - param$1 | 0;
              }), param, param$1);
}

function $star(param, param$1) {
  return propagate_none((function (param, param$1) {
                return Math.imul(param, param$1);
              }), param, param$1);
}

function $slash(param, param$1) {
  return propagate_none((function (x, y) {
                if (y === 0) {
                  return ;
                } else {
                  return Caml_int32.div(x, y);
                }
              }), param, param$1);
}

exports.log = log;
exports.log2 = log2;
exports.plus_opt = plus_opt;
exports.minus_opt = minus_opt;
exports.mult_opt = mult_opt;
exports.div_opt = div_opt;
exports.propagate_none_try = propagate_none_try;
exports.propagate_none = propagate_none;
exports.wrap_output = wrap_output;
exports.$plus = $plus;
exports.$neg = $neg;
exports.$star = $star;
exports.$slash = $slash;
/* No side effect */