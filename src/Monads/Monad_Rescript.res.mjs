// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_promise from "rescript/lib/es6/Js_promise.js";
import * as Belt_Result from "rescript/lib/es6/Belt_Result.js";
import * as SafeReadFile from "./safeReadFile";

function produce(a, b) {
  return Promise.all([
    a,
    b
  ]);
}

function map(p, f) {
  return Js_promise.then_(v => Promise.resolve(f(v)), p);
}

function concatContent(c1, c2) {
  if (c1 !== undefined) {
    if (c2 !== undefined) {
      return c1 + c2;
    } else {
      return c1;
    }
  } else if (c2 !== undefined) {
    return c2;
  } else {
    return;
  }
}

function readAndConcatFiles(f1, f2) {
  return map(map(Promise.all([
    f1,
    f2
  ]), param => concatContent(param[0], param[1])), c => {
    console.log(c);
  });
}

function safeReadFile(prim) {
  return SafeReadFile(prim);
}

readAndConcatFiles(SafeReadFile("Color.res"), SafeReadFile("Result.res"));

console.log("=========");

function product(r1, r2) {
  if (r1.TAG === "Ok") {
    if (r2.TAG === "Ok") {
      return {
        TAG: "Ok",
        _0: [
          r1._0,
          r2._0
        ]
      };
    } else {
      return {
        TAG: "Error",
        _0: r2._0
      };
    }
  } else {
    return {
      TAG: "Error",
      _0: r1._0
    };
  }
}

let Result = {
  product: product
};

let maybeCityId = {
  TAG: "Ok",
  _0: 1
};

let maybeTargetUser = {
  TAG: "Ok",
  _0: 2
};

let maybeBookingIds = {
  TAG: "Ok",
  _0: 3
};

Belt_Result.map(Belt_Result.map(product(product(maybeCityId, maybeTargetUser), maybeBookingIds), param => {
  let match = param[0];
  return {
    cityId: match[0],
    targetUser: match[1],
    bookingIds: param[1]
  };
}), t => {
  console.log(t);
});

console.log("=========");

export {
  produce,
  map,
  concatContent,
  readAndConcatFiles,
  safeReadFile,
  Result,
  maybeCityId,
  maybeTargetUser,
  maybeBookingIds,
}
/*  Not a pure module */
