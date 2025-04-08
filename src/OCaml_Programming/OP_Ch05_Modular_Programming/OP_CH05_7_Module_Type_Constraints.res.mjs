// Generated by ReScript, PLEASE EDIT WITH CARE


function log(prim) {
  console.log(prim);
}

function $plus(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function $star(prim0, prim1) {
  return prim0 * prim1 | 0;
}

function $tilde$neg(prim) {
  return -prim | 0;
}

function to_string(__x) {
  return __x.toString();
}

let IntRing = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $star: $star,
  $tilde$neg: $tilde$neg,
  to_string: to_string
};

let prim = (2).toString();

console.log(prim);

function $plus$1(prim0, prim1) {
  return prim0 + prim1;
}

function $star$1(prim0, prim1) {
  return prim0 * prim1;
}

function $tilde$neg$1(prim) {
  return - prim;
}

function to_string$1(__x) {
  return __x.toString();
}

let FloatRing = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $star: $star$1,
  $tilde$neg: $tilde$neg$1,
  to_string: to_string$1
};

let START = {
  IntRing: IntRing,
  FloatRing: FloatRing
};

function $plus$2(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function $star$2(prim0, prim1) {
  return prim0 * prim1 | 0;
}

function $tilde$neg$2(prim) {
  return -prim | 0;
}

function to_string$2(prim) {
  return String(prim);
}

let IntRing$1 = {
  zero: 0,
  one: 1,
  $plus: $plus$2,
  $star: $star$2,
  $tilde$neg: $tilde$neg$2,
  to_string: to_string$2
};

function $plus$3(prim0, prim1) {
  return prim0 + prim1;
}

function $star$3(prim0, prim1) {
  return prim0 * prim1;
}

function $tilde$neg$3(prim) {
  return - prim;
}

function to_string$3(__x) {
  return __x.toString();
}

let FloatRing$1 = {
  zero: 0,
  one: 1,
  $plus: $plus$3,
  $star: $star$3,
  $tilde$neg: $tilde$neg$3,
  to_string: to_string$3
};

let B = {};

let C = {
  A: {}
};

export {
  log,
  START,
  IntRing$1 as IntRing,
  FloatRing$1 as FloatRing,
  B,
  C,
}
/* prim Not a pure module */
