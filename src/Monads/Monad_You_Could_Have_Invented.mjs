// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Stdlib__List from "@dsiu/rescript-stdlib-fp/src/Stdlib__List.mjs";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function f(x) {
  return x + 2.0;
}

function g(x) {
  return x * 3.0;
}

function f$p(x) {
  return [
    x + 2.0,
    "f was called."
  ];
}

function g$p(x) {
  return [
    x * 3.0,
    "g was called."
  ];
}

function f$pg$p(x) {
  let match = g$p(x);
  let match$1 = f$p(match[0]);
  return [
    match$1[0],
    match[1] + match$1[1]
  ];
}

let prim0 = f$pg$p(1.0);

console.log(prim0, "f'g'(1.0) = ");

function bind(f, param) {
  let match = f$p(param[0]);
  return [
    match[0],
    param[1] + match[1]
  ];
}

let prim0$1 = bind(f$p, [
  1.0 * 3.0,
  "g was called."
]);

console.log(prim0$1, "bind(f',g'(1.0)) = ");

let prim0$2 = bind(g$p, [
  1.0 + 2.0,
  "f was called."
]);

console.log(prim0$2, "bind(g',f'(1.0)) = ");

let prim0$3 = Stdlib__Function.compose(g$p, __x => bind(f$p, __x), 1.0);

console.log(prim0$3, "compose(g', bind(f'))(1.0) = ");

let prim0$4 = Stdlib__Function.compose(f$p, __x => bind(g$p, __x), 1.0);

console.log(prim0$4, "compose(g', bind(f'))(1.0) = ");

function unit(x) {
  return [
    x,
    ""
  ];
}

function lift(f, x) {
  return [
    f(x),
    ""
  ];
}

function lift$p(__x) {
  return Stdlib__Function.compose(f, unit, __x);
}

function liftThenCompose(x) {
  return bind(extra => [
    extra + 2.0,
    ""
  ], [
    x * 3.0,
    ""
  ]);
}

function composeLifted(extra) {
  return [
    Stdlib__Function.compose(g, f, extra),
    ""
  ];
}

let prim0$5 = liftThenCompose(13.0);

console.log(prim0$5, "liftThenCompose(13.0) = ");

let prim0$6 = composeLifted(13.0);

console.log(prim0$6, "composeLifted(13.0) = ");

function sqrt(x) {
  return Math.pow(x, 1.0 / 2.0);
}

function cbrt(x) {
  return Math.pow(x, 1.0 / 3.0);
}

function sqrt$p(x) {
  return {
    hd: sqrt(x),
    tl: {
      hd: sqrt(x + 1.0),
      tl: /* [] */0
    }
  };
}

function cbrt$p(x) {
  return {
    hd: cbrt(x),
    tl: {
      hd: cbrt(x + 1.0),
      tl: {
        hd: cbrt(x + 2.0),
        tl: /* [] */0
      }
    }
  };
}

function bindComplex(f, x) {
  return Stdlib__List.concatMany(Stdlib__List.toArray(Stdlib__List.map(x, f)));
}

function unitComplex(x) {
  return {
    hd: x,
    tl: /* [] */0
  };
}

let prim0$7 = Stdlib__List.toArray(bindComplex(sqrt$p, cbrt$p(2.0)));

console.log(prim0$7, "ddd");

let compose = Stdlib__Function.compose;

export {
  log,
  log2,
  compose,
  f,
  g,
  f$p,
  g$p,
  f$pg$p,
  bind,
  unit,
  lift,
  lift$p,
  liftThenCompose,
  composeLifted,
  sqrt,
  cbrt,
  sqrt$p,
  cbrt$p,
  bindComplex,
  unitComplex,
}
/* prim0 Not a pure module */
