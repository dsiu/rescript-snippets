// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function Extend(Arg) {
  var iter = function (t, f) {
    return Curry._3(Arg.fold, t, undefined, (function (param, a) {
                  return Curry._1(f, a);
                }));
  };
  var length = function (t) {
    return Curry._3(Arg.fold, t, 0, (function (acc, param) {
                  return acc + 1 | 0;
                }));
  };
  var count = function (t, f) {
    return Curry._3(Arg.fold, t, 0, (function (count, x) {
                  return count + (
                          Curry._1(f, x) ? 1 : 0
                        ) | 0;
                }));
  };
  var Short_circuit = /* @__PURE__ */Caml_exceptions.create("Foldable.Extend(Arg).Short_circuit");
  var for_all = function (c, f) {
    try {
      iter(c, (function (x) {
              if (Curry._1(f, x)) {
                return ;
              }
              throw {
                    RE_EXN_ID: Short_circuit,
                    Error: new Error()
                  };
            }));
      return true;
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === Short_circuit) {
        return false;
      }
      throw exn;
    }
  };
  var exists = function (c, f) {
    try {
      iter(c, (function (x) {
              if (!Curry._1(f, x)) {
                return ;
              }
              throw {
                    RE_EXN_ID: Short_circuit,
                    Error: new Error()
                  };
            }));
      return false;
    }
    catch (raw_exn){
      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
      if (exn.RE_EXN_ID === Short_circuit) {
        return true;
      }
      throw exn;
    }
  };
  return {
          iter: iter,
          length: length,
          count: count,
          for_all: for_all,
          exists: exists
        };
}

export {
  Extend ,
  
}
/* No side effect */
