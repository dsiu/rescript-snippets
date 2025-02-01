// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/List.js";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

function is_empty(list) {
  return list === 0;
}

function push(x, list) {
  return {
    hd: x,
    tl: list
  };
}

let Empty = /* @__PURE__ */Primitive_exceptions.create("OP_CH05_4_Encapsulation.ORIG.ListStack.Empty");

function peek(list) {
  if (list !== 0) {
    return list.hd;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

function pop(list) {
  if (list !== 0) {
    return list.tl;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

let size = List.length;

let ListStack = {
  Empty: Empty,
  empty: /* [] */0,
  is_empty: is_empty,
  push: push,
  peek: peek,
  pop: pop,
  size: size
};

let ORIG = {
  ListStack: ListStack
};

let Empty$1 = /* @__PURE__ */Primitive_exceptions.create("OP_CH05_4_Encapsulation.NEW.ListStackCachedSize.Empty");

function is_empty$1(stack) {
  return stack[0] === 0;
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
  let match = stack[0];
  if (match !== 0) {
    return match.hd;
  }
  throw {
    RE_EXN_ID: Empty$1,
    Error: new Error()
  };
}

function pop$1(stack) {
  let match = stack[0];
  if (match !== 0) {
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

function size$1(prim) {
  return prim[1];
}

let ListStackCachedSize_empty = [
  /* [] */0,
  0
];

let ListStackCachedSize = {
  Empty: Empty$1,
  empty: ListStackCachedSize_empty,
  is_empty: is_empty$1,
  push: push$1,
  peek: peek$1,
  pop: pop$1,
  size: size$1
};

function is_empty$2(list) {
  return list === 0;
}

function push$2(x, list) {
  return {
    hd: x,
    tl: list
  };
}

let Empty$2 = /* @__PURE__ */Primitive_exceptions.create("OP_CH05_4_Encapsulation.NEW.ListStack.Empty");

function peek$2(list) {
  if (list !== 0) {
    return list.hd;
  }
  throw {
    RE_EXN_ID: Empty$2,
    Error: new Error()
  };
}

function pop$2(list) {
  if (list !== 0) {
    return list.tl;
  }
  throw {
    RE_EXN_ID: Empty$2,
    Error: new Error()
  };
}

let size$2 = List.length;

let ListStack$1 = {
  Empty: Empty$2,
  empty: /* [] */0,
  is_empty: is_empty$2,
  push: push$2,
  peek: peek$2,
  pop: pop$2,
  size: size$2
};

let NEW = {
  ListStackCachedSize: ListStackCachedSize,
  ListStack: ListStack$1
};

export {
  ORIG,
  NEW,
}
/* No side effect */
