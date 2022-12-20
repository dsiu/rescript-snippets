// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml from "rescript/lib/es6/caml.js";
import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_Id from "rescript/lib/es6/belt_Id.js";
import * as Belt_Map from "rescript/lib/es6/belt_Map.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_HashMap from "rescript/lib/es6/belt_HashMap.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Stdlib_Function from "../../stdlib/Stdlib_Function.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function IncX(M) {
  var x = M.x + 1 | 0;
  return {
          x: x
        };
}

var A = {
  x: 0
};

var x = 1;

var B = {
  x: x
};

var x$1 = x + 1 | 0;

var C = {
  x: x$1
};

console.log("B.x", x);

console.log("C.x", x$1);

function AddX(M) {
  var add = function (y) {
    return M.x + y | 0;
  };
  return {
          add: add
        };
}

function add(y) {
  return 42 + y | 0;
}

var Add42 = {
  add: add
};

var prim1 = 43;

console.log("Add42.add(1)", prim1);

function Pair1(M) {
  var p_0 = M.x;
  var p = [
    p_0,
    1
  ];
  return {
          p: p
        };
}

function Pair1_(M) {
  var p_0 = M.x;
  var p = [
    p_0,
    1
  ];
  return {
          p: p
        };
}

var p = [
  0,
  1
];

var P0 = {
  p: p
};

var p$1 = [
  /* 'a' */97,
  1
];

var PA = {
  p: p$1
};

function F(M) {
  return {
          y: M.x
        };
}

var X = {
  x: 0
};

var Z = {
  x: 0,
  z: 0
};

var FX = {
  y: 0
};

var FZ = {
  y: 0
};

function hash(a) {
  return a;
}

var eq = Caml_obj.equal;

var IntHash = Belt_Id.MakeHashable({
      hash: hash,
      eq: eq
    });

var hMap = Belt_HashMap.make(10, IntHash);

Belt_HashMap.set(hMap, 0, "a");

function cmp(param, param$1) {
  var c = Caml.string_compare(param.last, param$1.last);
  if (c !== 0) {
    return c;
  } else {
    return Caml.string_compare(param.first, param$1.first);
  }
}

var Name = Belt_Id.MakeComparable({
      cmp: cmp
    });

var empty = Belt_Map.make(Name);

var nm = Belt_Map.set(empty, {
      first: "danny",
      last: "siu"
    }, 1970);

var Empty = /* @__PURE__ */Caml_exceptions.create("OP_CH05_9_Functors.Empty");

function peek(s) {
  if (s) {
    return s.hd;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function pop(s) {
  if (s) {
    return s.tl;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

var ListStack = {
  empty: /* [] */0,
  push: List.cons,
  peek: peek,
  pop: pop
};

function push(x, s) {
  return /* S */{
          _0: x,
          _1: s
        };
}

function peek$1(s) {
  if (s) {
    return s._0;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function pop$1(s) {
  if (s) {
    return s._1;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

var VariantStack = {
  empty: /* E */0,
  push: push,
  peek: peek$1,
  pop: pop$1
};

var prim = peek(List.cons(1, /* [] */0)) === 1;

console.log(prim);

var prim$1 = peek$1(/* S */{
      _0: 1,
      _1: /* E */0
    }) === 1;

console.log(prim$1);

function StackTester(S) {
  var tests = Curry._1(S.peek, Curry._2(S.push, 1, S.empty)) === 1;
  return {
          tests: tests
        };
}

var tests = peek(List.cons(1, /* [] */0)) === 1;

var ListStackTester = {
  tests: tests
};

var tests$1 = peek$1(/* S */{
      _0: 1,
      _1: /* E */0
    }) === 1;

var VariantStackTester = {
  tests: tests$1
};

var stacks_1 = {
  hd: VariantStack,
  tl: /* [] */0
};

var stacks = {
  hd: ListStack,
  tl: stacks_1
};

function tests$2(m) {
  var tests$3 = Curry._1(m.peek, Curry._2(m.push, 1, m.empty)) === 1;
  return tests$3;
}

var prim$2 = List.map(tests$2, stacks);

console.log(prim$2);

function SetOfList(S) {
  var of_list = function (lst) {
    return List.fold_right(S.add, lst, S.empty);
  };
  return {
          of_list: of_list
        };
}

function elements(s) {
  return List.sort_uniq(Caml_obj.compare, s);
}

var ListSet = {
  empty: /* [] */0,
  mem: List.mem,
  add: List.cons,
  elements: elements
};

function add$1(x, s) {
  if (List.mem(x, s)) {
    return s;
  } else {
    return {
            hd: x,
            tl: s
          };
  }
}

var UniqListSet = {
  empty: /* [] */0,
  mem: List.mem,
  add: add$1,
  elements: Stdlib_Function.identity
};

function of_list(lst) {
  return List.fold_right(List.cons, lst, /* [] */0);
}

var OfList = {
  of_list: of_list
};

function of_list$1(lst) {
  return List.fold_right(add$1, lst, /* [] */0);
}

var UniqOfList = {
  of_list: of_list$1
};

function SetWithOfList(S) {
  var of_list = function (lst) {
    return List.fold_right(S.add, lst, S.empty);
  };
  return {
          empty: S.empty,
          mem: S.mem,
          add: S.add,
          elements: S.elements,
          of_list: of_list
        };
}

function of_list$2(lst) {
  return List.fold_right(List.cons, lst, /* [] */0);
}

var SetL = {
  empty: /* [] */0,
  mem: List.mem,
  add: List.cons,
  elements: elements,
  of_list: of_list$2
};

function of_list$3(lst) {
  return List.fold_right(add$1, lst, /* [] */0);
}

var UniqSetL = {
  empty: /* [] */0,
  mem: List.mem,
  add: add$1,
  elements: Stdlib_Function.identity,
  of_list: of_list$3
};

var CheckAddX = AddX;

export {
  log ,
  log2 ,
  IncX ,
  A ,
  B ,
  C ,
  AddX ,
  Add42 ,
  CheckAddX ,
  Pair1 ,
  Pair1_ ,
  P0 ,
  PA ,
  F ,
  X ,
  Z ,
  FX ,
  FZ ,
  IntHash ,
  hMap ,
  Name ,
  empty ,
  nm ,
  Empty ,
  ListStack ,
  VariantStack ,
  StackTester ,
  ListStackTester ,
  VariantStackTester ,
  stacks ,
  tests$2 as tests,
  SetOfList ,
  ListSet ,
  UniqListSet ,
  OfList ,
  UniqOfList ,
  SetWithOfList ,
  SetL ,
  UniqSetL ,
}
/*  Not a pure module */
