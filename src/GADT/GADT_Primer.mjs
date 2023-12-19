// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";

function mapOption$p(f, opt) {
  if (typeof opt !== "object") {
    return "None'";
  } else {
    return {
            TAG: "Some'",
            _0: Curry._1(f, opt._0)
          };
  }
}

var a = {
  TAG: "Some'",
  _0: 5
};

function inc(x) {
  return x + 1 | 0;
}

var c = mapOption$p(inc, a);

var d = mapOption$p(inc, "None'");

function $$eval(a) {
  return a.VAL;
}

var myInt = 42;

var myFloat = 4.2;

var myBool = false;

var myStr = "Hello";

var List$p = {};

var HList = {};

var myHList = {
  TAG: "Cons",
  _0: 1,
  _1: {
    TAG: "Cons",
    _0: "a",
    _1: {
      TAG: "Cons",
      _0: 1.5,
      _1: "Nil"
    }
  }
};

console.log(myHList);

var b = "None'";

var myList = {
  TAG: "Con",
  _0: 1,
  _1: {
    TAG: "Con",
    _0: 2,
    _1: {
      TAG: "Con",
      _0: 3,
      _1: "Empty"
    }
  }
};

var hList = [
  1,
  [
    "Hello",
    [
      1.234,
      undefined
    ]
  ]
];

export {
  mapOption$p ,
  a ,
  b ,
  inc ,
  c ,
  d ,
  $$eval ,
  myInt ,
  myFloat ,
  myBool ,
  myStr ,
  List$p ,
  myList ,
  HList ,
  myHList ,
  hList ,
}
/* c Not a pure module */
