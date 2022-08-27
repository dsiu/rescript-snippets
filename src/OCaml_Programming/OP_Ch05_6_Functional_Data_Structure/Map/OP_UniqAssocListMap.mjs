// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";

function insert(k, v, m) {
  return {
          hd: [
            k,
            v
          ],
          tl: List.remove_assoc(k, m)
        };
}

var lookup = List.assoc;

function bindings(m) {
  return m;
}

var M = {
  empty: /* [] */0,
  insert: insert,
  lookup: lookup,
  bindings: bindings
};

export {
  M ,
}
/* No side effect */
