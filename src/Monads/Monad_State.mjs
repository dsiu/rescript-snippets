// Generated by ReScript, PLEASE EDIT WITH CARE


function rng_0(s) {
  let rand = Math.random() - 0.5;
  return [
    s + rand,
    rand
  ];
}

function rbg_0(s) {
  let match = rng_0(s);
  return [
    match[0],
    match[1] > 0.0
  ];
}

console.log(rng_0(3.0));

console.log(rbg_0(2.0));

let match = rng_0(2.0);

let r0 = match[1];

let s1 = match[0];

let match$1 = rng_0(s1);

let r1 = match$1[1];

let s2 = match$1[0];

let match$2 = rng_0(s2);

let r2 = match$2[1];

console.log(r0 + r1 + r2);

function rng(s) {
  let rand = Math.random() - 0.5;
  return {
    TAG: "State",
    _0: s + rand,
    _1: rand
  };
}

function rbg(s) {
  let match = rng(s);
  return {
    TAG: "State",
    _0: match._0,
    _1: match._1 > 0.0
  };
}

let nextLong = rng;

let nextBool = rbg;

console.log(rng(1.0));

console.log(rng(2.0));

console.log(rng(3.0));

console.log(rbg(1.0));

console.log(rbg(2.0));

console.log(rbg(3.0));

console.log("Use re-fp (not working, should use Relude)");

let s0 = 2.0;

export {
  rng_0,
  rbg_0,
  s0,
  s1,
  r0,
  s2,
  r1,
  r2,
  rng,
  rbg,
  nextLong,
  nextBool,
}
/*  Not a pure module */
