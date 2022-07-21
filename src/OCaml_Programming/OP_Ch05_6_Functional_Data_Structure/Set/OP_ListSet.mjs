// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";

function elements(s) {
  return List.sort_uniq(Caml_obj.caml_compare, s);
}

var M = {
  empty: /* [] */0,
  mem: List.mem,
  add: List.cons,
  elements: elements
};

export {
  M ,
  
}
/* No side effect */