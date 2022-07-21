// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as $$String from "rescript/lib/es6/string.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_Array from "rescript/lib/es6/belt_Array.js";
import * as Caml_string from "rescript/lib/es6/caml_string.js";

function log(prim) {
  console.log(prim);
  
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
  
}

function strToChar(__x) {
  return Caml_string.get(__x, 0);
}

function charToStr(__x) {
  return $$String.make(1, __x);
}

function parseA(str) {
  if (str.length === 0) {
    return [
            false,
            ""
          ];
  }
  if (Caml_string.get(str.charAt(0), 0) !== /* 'A' */65) {
    return [
            false,
            str
          ];
  }
  var remaining = str.slice(1);
  return [
          true,
          remaining
        ];
}

var inputABC = "ABC";

var prim1 = parseA(inputABC);

console.log("parseA", prim1);

var inputZBC = "ZBC";

var prim1$1 = parseA(inputZBC);

console.log("parseA", prim1$1);

console.log("-- Parsing a specified character");

function pchar(charToMatch, str) {
  if (str.length === 0) {
    return [
            "No more input",
            ""
          ];
  }
  var first = Caml_string.get(str.charAt(0), 0);
  if (first === charToMatch) {
    var remaining = str.slice(1);
    var msg = "Found " + $$String.make(1, charToMatch);
    return [
            msg,
            remaining
          ];
  }
  var msg$1 = "Expecting '" + $$String.make(1, charToMatch) + "'. Got '" + $$String.make(1, first) + "'";
  return [
          msg$1,
          str
        ];
}

var prim1$2 = pchar(/* 'A' */65, inputABC);

console.log("pchar", prim1$2);

var prim1$3 = pchar(/* 'A' */65, inputZBC);

console.log("pchar", prim1$3);

var prim1$4 = pchar(/* 'Z' */90, inputZBC);

console.log("pchar", prim1$4);

console.log("-- Returning a Success/Failure");

function pchar$1(charToMatch, str) {
  if (str.length === 0) {
    return {
            TAG: /* Failure */1,
            _0: "No more input"
          };
  }
  var first = Caml_string.get(str.charAt(0), 0);
  if (first === charToMatch) {
    var remaining = str.slice(1);
    return {
            TAG: /* Success */0,
            _0: [
              charToMatch,
              remaining
            ]
          };
  }
  var msg = "Expecting '" + $$String.make(1, charToMatch) + "'. Got '" + $$String.make(1, first) + "'";
  return {
          TAG: /* Failure */1,
          _0: msg
        };
}

var prim1$5 = pchar$1(/* 'A' */65, inputABC);

console.log("pchar", prim1$5);

var prim1$6 = pchar$1(/* 'A' */65, inputZBC);

console.log("pchar", prim1$6);

console.log("-- Rewriting with an inner function");

function pchar$2(charToMatch) {
  return function (str) {
    if (str.length === 0) {
      return {
              TAG: /* Failure */1,
              _0: "No more input"
            };
    }
    var first = Caml_string.get(str.charAt(0), 0);
    if (first === charToMatch) {
      var remaining = str.slice(1);
      return {
              TAG: /* Success */0,
              _0: [
                charToMatch,
                remaining
              ]
            };
    }
    var msg = "Expecting '" + $$String.make(1, charToMatch) + "'. Got '" + $$String.make(1, first) + "'";
    return {
            TAG: /* Failure */1,
            _0: msg
          };
  };
}

console.log("-- The benefits of the curried implementation");

var parseA$1 = pchar$2(/* 'A' */65);

var prim1$7 = Curry._1(parseA$1, inputABC);

console.log("parseA", prim1$7);

var prim1$8 = Curry._1(parseA$1, inputZBC);

console.log("parseA", prim1$8);

console.log("Encapsulating the parsing function in a type");

function pchar$3(charToMatch) {
  var innerFn = function (str) {
    if (str.length === 0) {
      return {
              TAG: /* Failure */1,
              _0: "No more input"
            };
    }
    var first = Caml_string.get(str.charAt(0), 0);
    if (first === charToMatch) {
      var remaining = str.slice(1);
      return {
              TAG: /* Success */0,
              _0: [
                charToMatch,
                remaining
              ]
            };
    }
    var msg = "Expecting '" + $$String.make(1, charToMatch) + "'. Got '" + $$String.make(1, first) + "'";
    return {
            TAG: /* Failure */1,
            _0: msg
          };
  };
  return /* Parser */{
          _0: innerFn
        };
}

console.log("-- Testing the wrapped function");

function run(parser, input) {
  return Curry._1(parser._0, input);
}

var parseA$2 = pchar$3(/* 'A' */65);

var prim1$9 = run(parseA$2, inputABC);

console.log("parseA", prim1$9);

var prim1$10 = run(parseA$2, inputZBC);

console.log("parseA", prim1$10);

console.log("-- Combining two parsers in sequence");

function andThen(parser1, parser2) {
  var innerFn = function (input) {
    var result1 = run(parser1, input);
    if (result1.TAG !== /* Success */0) {
      return {
              TAG: /* Failure */1,
              _0: result1._0
            };
    }
    var match = result1._0;
    var result2 = run(parser2, match[1]);
    if (result2.TAG !== /* Success */0) {
      return {
              TAG: /* Failure */1,
              _0: result2._0
            };
    }
    var match$1 = result2._0;
    var newValue_0 = match[0];
    var newValue_1 = match$1[0];
    var newValue = [
      newValue_0,
      newValue_1
    ];
    return {
            TAG: /* Success */0,
            _0: [
              newValue,
              match$1[1]
            ]
          };
  };
  return /* Parser */{
          _0: innerFn
        };
}

console.log("-- Testing andThen");

var parseA$3 = pchar$3(/* 'A' */65);

var parseB = pchar$3(/* 'B' */66);

var parseAThenB = andThen(parseA$3, parseB);

var prim1$11 = run(parseAThenB, "ABC");

console.log("parseAThenB ABC", prim1$11);

var prim1$12 = run(parseAThenB, "ZBC");

console.log("parseAThenB ZBC", prim1$12);

var prim1$13 = run(parseAThenB, "AZC");

console.log("parseAThenB AZC", prim1$13);

console.log("-- Choosing between two parsers");

function orElse(parser1, parser2) {
  var innerFn = function (input) {
    var result1 = run(parser1, input);
    if (result1.TAG === /* Success */0) {
      return result1;
    } else {
      return run(parser2, input);
    }
  };
  return /* Parser */{
          _0: innerFn
        };
}

console.log("-- Testing orElse");

var parseA$4 = pchar$3(/* 'A' */65);

var parseB$1 = pchar$3(/* 'B' */66);

var parseAOrElseB = orElse(parseA$4, parseB$1);

var prim1$14 = run(parseAOrElseB, "AZZ");

console.log("parseAOrElseB AZZ", prim1$14);

var prim1$15 = run(parseAOrElseB, "BZZ");

console.log("parseAOrElseB BZZ", prim1$15);

var prim1$16 = run(parseAOrElseB, "CZZ");

console.log("parseAOrElseB CZZ", prim1$16);

console.log("-- Combining andThen and orElse");

var parseA$5 = pchar$3(/* 'A' */65);

var parseB$2 = pchar$3(/* 'B' */66);

var parseC = pchar$3(/* 'C' */67);

var bOrElseC = orElse(parseB$2, parseC);

var aAndThenBorC = andThen(parseA$5, bOrElseC);

var prim1$17 = run(aAndThenBorC, "ABZ");

console.log("aAndThenBorC ABZ", prim1$17);

var prim1$18 = run(aAndThenBorC, "ACZ");

console.log("aAndThenBorC ACZ", prim1$18);

var prim1$19 = run(aAndThenBorC, "QBZ");

console.log("aAndThenBorC QBZ", prim1$19);

var prim1$20 = run(aAndThenBorC, "AQZ");

console.log("aAndThenBorC AQZ", prim1$20);

console.log("-- Choosing from a list of parsers");

function choice(listOfParsers) {
  return Belt_List.reduce(listOfParsers, /* Parser */{
              _0: (function (string) {
                  return {
                          TAG: /* Failure */1,
                          _0: "Initial parser"
                        };
                })
            }, orElse);
}

function anyOf(listOfChars) {
  return choice(Belt_List.map(listOfChars, pchar$3));
}

var listOfChars = Belt_List.fromArray(Belt_Array.map("abcdefghijklmnopqrstuvwxyz".split(""), strToChar));

var parseLowercase = choice(Belt_List.map(listOfChars, pchar$3));

var listOfChars$1 = Belt_List.fromArray(Belt_Array.map("0123456789".split(""), strToChar));

var parseDigit = choice(Belt_List.map(listOfChars$1, pchar$3));

var prim1$21 = run(parseLowercase, "aBC");

console.log("parseLowercase aBC", prim1$21);

var prim1$22 = run(parseLowercase, "ABC");

console.log("parseLowercase ABC", prim1$22);

var prim1$23 = run(parseDigit, "1ABC");

console.log("parseDigit 1ABC", prim1$23);

var prim1$24 = run(parseDigit, "9ABC");

console.log("parseDigit 9ABC", prim1$24);

var prim1$25 = run(parseDigit, "|ABC");

console.log("parseDigit |ABC", prim1$25);

export {
  log ,
  log2 ,
  strToChar ,
  charToStr ,
  inputABC ,
  inputZBC ,
  pchar$3 as pchar,
  run ,
  andThen ,
  parseAThenB ,
  orElse ,
  parseAOrElseB ,
  parseA$5 as parseA,
  parseB$2 as parseB,
  parseC ,
  bOrElseC ,
  aAndThenBorC ,
  choice ,
  anyOf ,
  parseLowercase ,
  parseDigit ,
  
}
/* prim1 Not a pure module */
