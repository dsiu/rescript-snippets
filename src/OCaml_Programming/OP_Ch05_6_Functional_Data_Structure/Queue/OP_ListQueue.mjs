// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";

var Empty = /* @__PURE__ */Caml_exceptions.create("OP_ListQueue.M.Empty");

function is_empty(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function enqueue(x, q) {
  return Pervasives.$at(q, {
              hd: x,
              tl: /* [] */0
            });
}

function front(x) {
  if (x) {
    return x.hd;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function dequeue(x) {
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
  enqueue: enqueue,
  front: front,
  dequeue: dequeue,
  size: List.length,
  to_list: Stdlib__Function.identity
};

export {
  M ,
}
/* No side effect */
