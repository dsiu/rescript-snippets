// Generated by ReScript, PLEASE EDIT WITH CARE


function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function true_(a, b) {
  return a;
}

function false_(a, b) {
  return b;
}

function not_(b) {
  return b(false_, true_);
}

let prim = true_(false_, true_)("true", "false");

console.log(prim);

let prim$1 = false_(false_, true_)("true", "false");

console.log(prim$1);

export {
  log,
  log2,
  true_,
  false_,
  not_,
}
/* prim Not a pure module */
