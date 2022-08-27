// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_option from "rescript/lib/es6/caml_option.js";

function saveThing(t) {
  console.log(t);
  return Caml_option.some(t);
}

function maybeSaveThing(maybeThing) {
  if (maybeThing !== undefined) {
    return saveThing(Caml_option.valFromOption(maybeThing));
  }
  
}

maybeSaveThing(5);

maybeSaveThing("Siu");

export {
  saveThing ,
  maybeSaveThing ,
}
/*  Not a pure module */
