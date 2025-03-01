// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

let Empty = /* @__PURE__ */Primitive_exceptions.create("OP_Ch05_4_2_Abstract_Types.ListStack.Empty");

function is_empty(s) {
  return s === 0;
}

function push(x, s) {
  return {
    hd: x,
    tl: s
  };
}

function peek(s) {
  if (s !== 0) {
    return s.hd;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

function pop(s) {
  if (s !== 0) {
    return s.tl;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

let size = Stdlib_List.length;

let ListStack = {
  Empty: Empty,
  empty: /* [] */0,
  is_empty: is_empty,
  push: push,
  peek: peek,
  pop: pop,
  size: size
};

let Empty$1 = /* @__PURE__ */Primitive_exceptions.create("OP_Ch05_4_2_Abstract_Types.ListStackCachedSize.Empty");

function is_empty$1(s) {
  return s[0] === 0;
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
  let match = s[0];
  if (match !== 0) {
    return match.hd;
  }
  throw {
    RE_EXN_ID: Empty$1,
    Error: new Error()
  };
}

function pop$1(s) {
  let match = s[0];
  if (match !== 0) {
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

let Empty$2 = /* @__PURE__ */Primitive_exceptions.create("OP_Ch05_4_2_Abstract_Types.CustomStack.Empty");

function is_empty$2(s) {
  return s._0 === undefined;
}

function size$2(s) {
  let match = s._0;
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
  let match = s._0;
  if (match !== undefined) {
    return match.top;
  }
  throw {
    RE_EXN_ID: Empty$2,
    Error: new Error()
  };
}

function pop$2(s) {
  let match = s._0;
  if (match !== undefined) {
    return match.rest;
  }
  throw {
    RE_EXN_ID: Empty$2,
    Error: new Error()
  };
}

let CustomStack_empty = {
  TAG: "S",
  _0: undefined
};

let CustomStack = {
  Empty: Empty$2,
  empty: CustomStack_empty,
  is_empty: is_empty$2,
  push: push$2,
  peek: peek$2,
  pop: pop$2,
  size: size$2
};

let CheckListStackCachedSize = ListStackCachedSize;

let $great$great$eq = Belt_Option.flatMap;

export {
  ListStack,
  ListStackCachedSize,
  CheckListStackCachedSize,
  CustomStack,
  $great$great$eq,
}
/* No side effect */
