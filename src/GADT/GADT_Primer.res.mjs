// Generated by ReScript, PLEASE EDIT WITH CARE


function mapOption$p(f, opt) {
  if (typeof opt !== "object") {
    return "None'";
  } else {
    return {
      TAG: "Some'",
      _0: f(opt._0)
    };
  }
}

let a = {
  TAG: "Some'",
  _0: 5
};

function inc(x) {
  return x + 1 | 0;
}

let c = mapOption$p(inc, a);

let d = mapOption$p(inc, "None'");

function $$eval(a) {
  return a.VAL;
}

let myInt = 42;

let myFloat = 4.2;

let myBool = false;

let myStr = "Hello";

let List$p = {};

let HList = {};

let myHList = {
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

let b = "None'";

let myList = {
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

let hList = [
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
  mapOption$p,
  a,
  b,
  inc,
  c,
  d,
  $$eval,
  myInt,
  myFloat,
  myBool,
  myStr,
  List$p,
  myList,
  HList,
  myHList,
  hList,
}
/* c Not a pure module */
