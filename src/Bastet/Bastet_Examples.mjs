// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Result$BsBastet from "bs-bastet/src/Result.mjs";
import * as Function$BsBastet from "bs-bastet/src/Function.mjs";

function Hush(B) {
  var hush = function (bifunctor) {
    return B.bimap(Function$BsBastet.Category.id, (function (__x) {
                  Function$BsBastet.$$const(undefined, __x);
                }), bifunctor);
  };
  return {
          hush: hush
        };
}

function hush(bifunctor) {
  return Result$BsBastet.Bifunctor.bimap(Function$BsBastet.Category.id, (function (__x) {
                Function$BsBastet.$$const(undefined, __x);
              }), bifunctor);
}

var Hush_Result = {
  hush: hush
};

export {
  Hush ,
  Hush_Result ,
  hush ,
}
/* Result-BsBastet Not a pure module */
