// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Stdlib_Function from "@dsiu/rescript-stdlib-fp/src/Stdlib_Function.mjs";

var Empty = /* @__PURE__ */Caml_exceptions.create("OP_ListStack.M.Empty");

function is_empty(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
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

var M = {
  Empty: Empty,
  empty: /* [] */0,
  is_empty: is_empty,
  push: List.cons,
  peek: peek,
  pop: pop,
  size: List.length,
  to_list: Stdlib_Function.identity
};

export {
  M ,
}
/* No side effect */
