// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/List.js";
import * as Option from "rescript/lib/es6/Option.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";

function insert(k, v, m) {
  return {
    hd: [
      k,
      v
    ],
    tl: List.removeAssoc(m, k, Primitive_object.equal)
  };
}

function lookup(k, m) {
  return Option.getExn(List.getAssoc(m, k, Primitive_object.equal), undefined);
}

function bindings(m) {
  return m;
}

let M = {
  empty: /* [] */0,
  insert: insert,
  lookup: lookup,
  bindings: bindings
};

export {
  M,
}
/* No side effect */
