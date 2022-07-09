// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var List = require("rescript/lib/js/list.js");
var FP_Utils = require("../../../FP_Utils.bs.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

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
  to_list: FP_Utils.id
};

exports.M = M;
/* No side effect */