// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

let Empty = /* @__PURE__ */Primitive_exceptions.create("OP_ListStack.M.Empty");

function is_empty(x) {
  return !x;
}

function push(x, xs) {
  return Stdlib__List.cons(xs, x);
}

function peek(x) {
  if (x) {
    return x.hd;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

function pop(x) {
  if (x) {
    return x.tl;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

function to_list(prim) {
  return prim;
}

let M = {
  Empty: Empty,
  empty: /* [] */0,
  is_empty: is_empty,
  push: push,
  peek: peek,
  pop: pop,
  size: Stdlib__List.length,
  to_list: to_list
};

export {
  M,
}
/* Stdlib__List Not a pure module */
