// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Test = require("rescript-test/src/Test.bs.js");
var FizzBuzz = require("../src/FizzBuzz.bs.js");
var Test_Utils = require("./Test_Utils.bs.js");

Test.test("FizzBuzz 12", (function (param) {
        return Test_Utils.stringEqual(undefined, FizzBuzz.fizzbuzz(12), "Fizz");
      }));

Test.test("FizzBuzz 10", (function (param) {
        return Test_Utils.stringEqual(undefined, FizzBuzz.fizzbuzz(10), "Buzz");
      }));

Test.test("FizzBuzz 15", (function (param) {
        return Test_Utils.stringEqual(undefined, FizzBuzz.fizzbuzz(15), "FizzBuzz");
      }));

Test.test("FizzBuzz 1", (function (param) {
        return Test_Utils.stringEqual(undefined, FizzBuzz.fizzbuzz(1), "1");
      }));

/*  Not a pure module */
