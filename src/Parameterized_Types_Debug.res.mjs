// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

function saveThing(t) {
  console.log(t);
  return Primitive_option.some(t);
}

function maybeSaveThing(maybeThing) {
  if (maybeThing !== undefined) {
    return saveThing(Primitive_option.valFromOption(maybeThing));
  }
  
}

maybeSaveThing(5);

maybeSaveThing("Siu");

export {
  saveThing,
  maybeSaveThing,
}
/*  Not a pure module */
