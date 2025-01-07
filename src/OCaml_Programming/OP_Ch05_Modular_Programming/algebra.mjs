// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";

function $plus(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function $tilde$neg(prim) {
  return -prim | 0;
}

function $star(prim0, prim1) {
  return Math.imul(prim0, prim1);
}

function to_string(prim) {
  return String(prim);
}

function of_int(n) {
  return n;
}

let IntRing = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string,
  of_int: of_int
};

function $plus$1(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function $tilde$neg$1(prim) {
  return -prim | 0;
}

function $star$1(prim0, prim1) {
  return Math.imul(prim0, prim1);
}

let $slash = Primitive_int.div;

function to_string$1(prim) {
  return String(prim);
}

function of_int$1(n) {
  return n;
}

let IntField = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  $slash: $slash,
  to_string: to_string$1,
  of_int: of_int$1
};

function $plus$2(prim0, prim1) {
  return prim0 + prim1;
}

function $tilde$neg$2(prim) {
  return - prim;
}

function $star$2(prim0, prim1) {
  return prim0 * prim1;
}

function to_string$2(__x) {
  return __x.toString();
}

function of_int$2(n) {
  return n;
}

let FloatRing = {
  zero: 0,
  one: 1,
  $plus: $plus$2,
  $tilde$neg: $tilde$neg$2,
  $star: $star$2,
  to_string: to_string$2,
  of_int: of_int$2
};

function $plus$3(prim0, prim1) {
  return prim0 + prim1;
}

function $tilde$neg$3(prim) {
  return - prim;
}

function $star$3(prim0, prim1) {
  return prim0 * prim1;
}

function $slash$1(prim0, prim1) {
  return prim0 / prim1;
}

function to_string$3(__x) {
  return __x.toString();
}

function of_int$3(n) {
  return n;
}

let FloatField = {
  zero: 0,
  one: 1,
  $plus: $plus$3,
  $tilde$neg: $tilde$neg$3,
  $star: $star$3,
  $slash: $slash$1,
  to_string: to_string$3,
  of_int: of_int$3
};

function $plus$4(param, param$1) {
  let d = param$1[1];
  let b = param[1];
  return [
    Math.imul(param[0], d) + Math.imul(param$1[0], b) | 0,
    Math.imul(b, d)
  ];
}

function $tilde$neg$4(param) {
  return [
    -param[0] | 0,
    param[1]
  ];
}

function $slash$2(param, param$1) {
  return [
    Math.imul(param[0], param$1[1]),
    Math.imul(param[1], param$1[0])
  ];
}

function $star$4(param, param$1) {
  return [
    Math.imul(param[0], param$1[0]),
    Math.imul(param[1], param$1[1])
  ];
}

function to_string$4(param) {
  return String(param[0]) + "/" + String(param[1]);
}

function of_int$4(n) {
  return [
    n,
    1
  ];
}

let IntRational_zero = [
  0,
  0
];

let IntRational_one = [
  1,
  1
];

let IntRational = {
  zero: IntRational_zero,
  one: IntRational_one,
  $plus: $plus$4,
  $tilde$neg: $tilde$neg$4,
  $star: $star$4,
  $slash: $slash$2,
  to_string: to_string$4,
  of_int: of_int$4
};

function $plus$5(param, param$1) {
  let d = param$1[1];
  let b = param[1];
  return [
    param[0] * d + param$1[0] * b,
    b * d
  ];
}

function $tilde$neg$5(param) {
  return [
    - param[0],
    param[1]
  ];
}

function $slash$3(param, param$1) {
  return [
    param[0] * param$1[1],
    param[1] * param$1[0]
  ];
}

function $star$5(param, param$1) {
  return [
    param[0] * param$1[0],
    param[1] * param$1[1]
  ];
}

function to_string$5(param) {
  return param[0].toString() + "/" + param[1].toString();
}

function of_int$5(n) {
  return [
    n,
    1
  ];
}

let FloatRational_zero = [
  0,
  0
];

let FloatRational_one = [
  1,
  1
];

let FloatRational = {
  zero: FloatRational_zero,
  one: FloatRational_one,
  $plus: $plus$5,
  $tilde$neg: $tilde$neg$5,
  $star: $star$5,
  $slash: $slash$3,
  to_string: to_string$5,
  of_int: of_int$5
};

export {
  IntRing,
  IntField,
  FloatRing,
  FloatField,
  IntRational,
  FloatRational,
}
/* No side effect */
