// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Primitive_exceptions from "rescript/lib/es6/Primitive_exceptions.js";

let Empty = /* @__PURE__ */Primitive_exceptions.create("OP_BatchedQueue.M.Empty");

function is_empty(x) {
  return x.o === 0;
}

function enqueue(x, q) {
  let o = q.o;
  if (o !== 0) {
    return {
      o: o,
      i: {
        hd: x,
        tl: q.i
      }
    };
  } else {
    return {
      o: {
        hd: x,
        tl: /* [] */0
      },
      i: /* [] */0
    };
  }
}

function front(x) {
  let match = x.o;
  if (match !== 0) {
    return match.hd;
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

function dequeue(x) {
  let match = x.o;
  if (match !== 0) {
    let t = match.tl;
    if (t !== 0) {
      return {
        o: t,
        i: x.i
      };
    } else {
      return {
        o: Stdlib_List.reverse(x.i),
        i: /* [] */0
      };
    }
  }
  throw {
    RE_EXN_ID: Empty,
    Error: new Error()
  };
}

function size(param) {
  return Stdlib_List.length(param.o) + Stdlib_List.length(param.i) | 0;
}

function to_list(param) {
  return Pervasives.$at(param.o, Stdlib_List.reverse(param.i));
}

let M_empty = {
  o: /* [] */0,
  i: /* [] */0
};

let M = {
  Empty: Empty,
  empty: M_empty,
  is_empty: is_empty,
  enqueue: enqueue,
  front: front,
  dequeue: dequeue,
  size: size,
  to_list: to_list
};

export {
  M,
}
/* No side effect */
