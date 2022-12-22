// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_exn from "rescript/lib/es6/js_exn.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Stdlib_List from "./Stdlib_List.mjs";
import * as Stdlib_Array from "./Stdlib_Array.mjs";

function getExnWithMessage(option, message) {
  if (option !== undefined) {
    return Caml_option.valFromOption(option);
  } else {
    return Js_exn.raiseError(message);
  }
}

function optionOr(a, b) {
  if (a !== undefined) {
    return a;
  } else {
    return b;
  }
}

function arrayToMayBe(__x) {
  return Stdlib_Array.get(__x, 0);
}

function listToMayBe(__x) {
  return Stdlib_List.get(__x, 0);
}

var keepU = Belt_Option.keepU;

var keep = Belt_Option.keep;

var forEachU = Belt_Option.forEachU;

var forEach = Belt_Option.forEach;

var getExn = Belt_Option.getExn;

var mapWithDefaultU = Belt_Option.mapWithDefaultU;

var mapWithDefault = Belt_Option.mapWithDefault;

var mapU = Belt_Option.mapU;

var map = Belt_Option.map;

var flatMapU = Belt_Option.flatMapU;

var flatMap = Belt_Option.flatMap;

var getWithDefault = Belt_Option.getWithDefault;

var orElse = Belt_Option.orElse;

var isSome = Belt_Option.isSome;

var isNone = Belt_Option.isNone;

var eqU = Belt_Option.eqU;

var eq = Belt_Option.eq;

var cmpU = Belt_Option.cmpU;

var cmp = Belt_Option.cmp;

var fromMaybe = Belt_Option.getWithDefault;

var A;

var L;

export {
  keepU ,
  keep ,
  forEachU ,
  forEach ,
  getExn ,
  mapWithDefaultU ,
  mapWithDefault ,
  mapU ,
  map ,
  flatMapU ,
  flatMap ,
  getWithDefault ,
  orElse ,
  isSome ,
  isNone ,
  eqU ,
  eq ,
  cmpU ,
  cmp ,
  getExnWithMessage ,
  optionOr ,
  fromMaybe ,
  A ,
  arrayToMayBe ,
  L ,
  listToMayBe ,
}
/* No side effect */
