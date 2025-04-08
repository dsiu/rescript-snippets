// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "rescript-test/src/Test.res.mjs";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Belt_MapString from "rescript/lib/es6/Belt_MapString.js";

function intEqual(message, a, b) {
  Test.assertion(message, "intEqual", (a, b) => a === b, a, b);
}

function boolEqual(message, a, b) {
  Test.assertion(message, "boolEqual", (a, b) => a === b, a, b);
}

function stringEqual(message, a, b) {
  Test.assertion(message, "stringEqual", (a, b) => a === b, a, b);
}

function stringMapEqual(message, a, b) {
  Test.assertion(message, "stringMapEqual", (a, b) => Belt_MapString.eq(a, b, (a, b) => a === b), a, b);
}

function stringArrayEqual(message, a, b) {
  Test.assertion(message, "stringArrayEqual", (a, b) => Belt_Array.eq(a, b, (a, b) => a === b), a, b);
}

function intArrayEqual(message, a, b) {
  Test.assertion(message, "intArrayEqual", (a, b) => Belt_Array.eq(a, b, (a, b) => a === b), a, b);
}

function listEqual(message, a, b) {
  Test.assertion(message, "listEqual", (a, b) => Belt_List.eq(a, b, (a, b) => a === b), a, b);
}

function optionEqual(message, a, b) {
  Test.assertion(message, "optionEqual", (a, b) => Belt_Option.eq(a, b, (a, b) => a === b), a, b);
}

function optionListEqual(message, a, b) {
  Test.assertion(message, "optionEqual", (a, b) => Belt_Option.eq(a, b, (a, b) => Belt_List.eq(a, b, (a, b) => a === b)), a, b);
}

export {
  intEqual,
  boolEqual,
  stringEqual,
  stringMapEqual,
  stringArrayEqual,
  intArrayEqual,
  listEqual,
  optionEqual,
  optionListEqual,
}
/* Test Not a pure module */
