// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Belt_Option from "rescript/lib/es6/belt_Option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var Empty = /* @__PURE__ */Caml_exceptions.create("OP_Ch05_4_2_Abstract_Types.ListStack.Empty");

function is_empty(s) {
  if (s) {
    return false;
  } else {
    return true;
  }
}

function push(x, s) {
  return {
          hd: x,
          tl: s
        };
}

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

var size = List.length;

var ListStack = {
  Empty: Empty,
  empty: /* [] */0,
  is_empty: is_empty,
  push: push,
  peek: peek,
  pop: pop,
  size: size
};

var Empty$1 = /* @__PURE__ */Caml_exceptions.create("OP_Ch05_4_2_Abstract_Types.ListStackCachedSize.Empty");

function is_empty$1(s) {
  if (s[0]) {
    return false;
  } else {
    return true;
  }
}

function push$1(x, param) {
  return [
          {
            hd: x,
            tl: param[0]
          },
          param[1] + 1 | 0
        ];
}

function peek$1(s) {
  var match = s[0];
  if (match) {
    return match.hd;
  }
  throw {
        RE_EXN_ID: Empty$1,
        Error: new Error()
      };
}

function pop$1(s) {
  var match = s[0];
  if (match) {
    return [
            match.tl,
            s[1] - 1 | 0
          ];
  }
  throw {
        RE_EXN_ID: Empty$1,
        Error: new Error()
      };
}

function size$1(prim) {
  return prim[1];
}

var ListStackCachedSize_empty = [
  /* [] */0,
  0
];

var ListStackCachedSize = {
  Empty: Empty$1,
  empty: ListStackCachedSize_empty,
  is_empty: is_empty$1,
  push: push$1,
  peek: peek$1,
  pop: pop$1,
  size: size$1
};

var Empty$2 = /* @__PURE__ */Caml_exceptions.create("OP_Ch05_4_2_Abstract_Types.CustomStack.Empty");

function is_empty$2(s) {
  return s._0 === undefined;
}

function size$2(s) {
  var match = s._0;
  if (match !== undefined) {
    return match.size;
  } else {
    return 0;
  }
}

function push$2(x, s) {
  return {
          TAG: "S",
          _0: {
            top: x,
            rest: s,
            size: size$2(s) + 1 | 0
          }
        };
}

function peek$2(s) {
  var match = s._0;
  if (match !== undefined) {
    return match.top;
  }
  throw {
        RE_EXN_ID: Empty$2,
        Error: new Error()
      };
}

function pop$2(s) {
  var match = s._0;
  if (match !== undefined) {
    return match.rest;
  }
  throw {
        RE_EXN_ID: Empty$2,
        Error: new Error()
      };
}

var CustomStack_empty = {
  TAG: "S",
  _0: undefined
};

var CustomStack = {
  Empty: Empty$2,
  empty: CustomStack_empty,
  is_empty: is_empty$2,
  push: push$2,
  peek: peek$2,
  pop: pop$2,
  size: size$2
};

var CheckListStackCachedSize = ListStackCachedSize;

var $great$great$eq = Belt_Option.flatMap;

export {
  ListStack ,
  ListStackCachedSize ,
  CheckListStackCachedSize ,
  CustomStack ,
  $great$great$eq ,
}
/* No side effect */
