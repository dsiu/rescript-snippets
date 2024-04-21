// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var Empty = /* @__PURE__ */Caml_exceptions.create("OP_BatchedQueue.M.Empty");

function is_empty(x) {
  if (x.o) {
    return false;
  } else {
    return true;
  }
}

function enqueue(x, q) {
  var o = q.o;
  if (o) {
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
  var match = x.o;
  if (match) {
    return match.hd;
  }
  throw {
        RE_EXN_ID: Empty,
        Error: new Error()
      };
}

function dequeue(x) {
  var match = x.o;
  if (match) {
    var t = match.tl;
    if (t) {
      return {
              o: t,
              i: x.i
            };
    } else {
      return {
              o: List.rev(x.i),
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
  return List.length(param.o) + List.length(param.i) | 0;
}

function to_list(param) {
  return PervasivesU.$at(param.o, List.rev(param.i));
}

var M_empty = {
  o: /* [] */0,
  i: /* [] */0
};

var M = {
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
  M ,
}
/* No side effect */
