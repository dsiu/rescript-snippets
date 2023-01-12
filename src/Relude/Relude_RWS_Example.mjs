// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Relude_RWST from "relude/src/Relude_RWST.mjs";
import * as Relude_Identity from "relude/src/Relude_Identity.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var RWST_M = Relude_RWST.WithMonad(Relude_Identity.Monad);

var RWST_MY = Curry._2(RWST_M.WithEnvAndStateAndLog, {}, {});

var RWST;

export {
  RWST ,
  log ,
  log2 ,
  RWST_M ,
  RWST_MY ,
}
/* RWST_M Not a pure module */
