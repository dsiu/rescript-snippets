// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";

function x(param) {
  return 1;
}

function y(param) {
  return Curry._1(B.y, undefined) + 1 | 0;
}

var A = {
  x: x,
  y: y
};

function x$1(param) {
  return Curry._1(x, undefined) + 2 | 0;
}

function y$1(param) {
  return 2;
}

var B = {
  x: x$1,
  y: y$1
};

export {
  A ,
  B ,
}
/* No side effect */
