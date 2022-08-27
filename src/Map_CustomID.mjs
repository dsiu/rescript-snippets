// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_Map from "rescript/lib/es6/belt_Map.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";

function make(x) {
  return x;
}

function getValue(t) {
  return t;
}

var cmp = Caml_obj.compare;

var SomeId = {
  make: make,
  getValue: getValue,
  cmp: cmp
};

var SomeIdCmp = Belt_Id.MakeComparable({
      cmp: cmp
    });

var empty = Belt_Map.make(SomeIdCmp);

var map = Belt_Map.set(empty, 1, "hello");

var someId = 1;

export {
  SomeId ,
  SomeIdCmp ,
  empty ,
  someId ,
  map ,
}
/* SomeIdCmp Not a pure module */
