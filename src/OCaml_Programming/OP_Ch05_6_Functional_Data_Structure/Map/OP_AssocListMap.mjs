// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";

function insert(k, v, m) {
  return {
          hd: [
            k,
            v
          ],
          tl: m
        };
}

var lookup = List.assoc;

function keys(m) {
  return List.sort_uniq(Caml_obj.compare, List.map((function (prim) {
                    return prim[0];
                  }), m));
}

function bindings(m) {
  return List.map((function (k) {
                return [
                        k,
                        List.assoc(k, m)
                      ];
              }), keys(m));
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
