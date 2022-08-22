// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

function is_empty(list) {
  if (list) {
    return false;
  } else {
    return true;
  }
}

function push(x, list) {
  return {
          hd: x,
          tl: list
        };
}

var Empty = /* @__PURE__ */Caml_exceptions.create("OP_CH05_4_Encapsulation.ORIG.ListStack.Empty");

function peek(list) {
  if (list) {
    return list.hd;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function pop(list) {
  if (list) {
    return list.tl;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

var ListStack = {
  Empty: Empty,
  empty: /* [] */0,
  is_empty: is_empty,
  push: push,
  peek: peek,
  pop: pop,
  size: List.length
};

var ORIG = {
  ListStack: ListStack
};

var Empty$1 = /* @__PURE__ */Caml_exceptions.create("OP_CH05_4_Encapsulation.NEW.ListStackCachedSize.Empty");

function is_empty$1(stack) {
  if (stack[0]) {
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
          param[1] - 1 | 0
        ];
}

function peek$1(stack) {
  var match = stack[0];
  if (match) {
    return match.hd;
  }
  throw {
        RE_EXN_ID: Empty$1,
        Error: new Error()
      };
}

function pop$1(stack) {
  var match = stack[0];
  if (match) {
    return [
            match.tl,
            stack[1] - 1 | 0
          ];
  }
  throw {
        RE_EXN_ID: Empty$1,
        Error: new Error()
      };
}

function size(prim) {
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
  size: size
};

function is_empty$2(list) {
  if (list) {
    return false;
  } else {
    return true;
  }
}

function push$2(x, list) {
  return {
          hd: x,
          tl: list
        };
}

var Empty$2 = /* @__PURE__ */Caml_exceptions.create("OP_CH05_4_Encapsulation.NEW.ListStack.Empty");

function peek$2(list) {
  if (list) {
    return list.hd;
  }
  throw {
        RE_EXN_ID: Empty$2,
        Error: new Error()
      };
}

function pop$2(list) {
  if (list) {
    return list.tl;
  }
  throw {
        RE_EXN_ID: Empty$2,
        Error: new Error()
      };
}

var ListStack$1 = {
  Empty: Empty$2,
  empty: /* [] */0,
  is_empty: is_empty$2,
  push: push$2,
  peek: peek$2,
  pop: pop$2,
  size: List.length
};

var NEW = {
  ListStackCachedSize: ListStackCachedSize,
  ListStack: ListStack$1
};

export {
  ORIG ,
  NEW ,
  
}
/* No side effect */
