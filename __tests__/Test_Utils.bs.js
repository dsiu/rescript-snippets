// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "rescript-test/src/Test.bs.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";

function intEqual(message, a, b) {
  return Test.assertion(message, "intEqual", (function (a, b) {
                return a === b;
              }), a, b);
}

function boolEqual(message, a, b) {
  return Test.assertion(message, "boolEqual", (function (a, b) {
                return a === b;
              }), a, b);
}

function stringEqual(message, a, b) {
  return Test.assertion(message, "stringEqual", (function (a, b) {
                return a === b;
              }), a, b);
}

function stringMapEqual(message, a, b) {
  return Test.assertion(message, "stringMapEqual", (function (a, b) {
                return Belt_MapString.eq(a, b, (function (a, b) {
                              return a === b;
                            }));
              }), a, b);
}

function stringArrayEqual(message, a, b) {
  return Test.assertion(message, "stringArrayEqual", (function (a, b) {
                return Belt_Array.eq(a, b, (function (a, b) {
                              return a === b;
                            }));
              }), a, b);
}

function intArrayEqual(message, a, b) {
  return Test.assertion(message, "intArrayEqual", (function (a, b) {
                return Belt_Array.eq(a, b, (function (a, b) {
                              return a === b;
                            }));
              }), a, b);
}

function listEqual(message, a, b) {
  return Test.assertion(message, "listEqual", (function (a, b) {
                return Belt_List.eq(a, b, (function (a, b) {
                              return a === b;
                            }));
              }), a, b);
}

function optionEqual(message, a, b) {
  return Test.assertion(message, "optionEqual", (function (a, b) {
                return Belt_Option.eq(a, b, (function (a, b) {
                              return a === b;
                            }));
              }), a, b);
}

function optionListEqual(message, a, b) {
  return Test.assertion(message, "optionEqual", (function (a, b) {
                return Belt_Option.eq(a, b, (function (a, b) {
                              return Belt_List.eq(a, b, (function (a, b) {
                                            return a === b;
                                          }));
                            }));
              }), a, b);
}

export {
  intEqual ,
  boolEqual ,
  stringEqual ,
  stringMapEqual ,
  stringArrayEqual ,
  intArrayEqual ,
  listEqual ,
  optionEqual ,
  optionListEqual ,
  
}
/* Test Not a pure module */
