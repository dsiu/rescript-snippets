// Generated by ReScript, PLEASE EDIT WITH CARE


function log(prim) {
  console.log(prim);
}

function g(n) {
  return n;
}

function f(n) {
  console.log("Hello World");
  return n + 1 | 0;
}

function map(f, l) {
  if (!l) {
    return /* [] */0;
  }
  let y = f(l.hd);
  return {
    hd: y,
    tl: map(f, l.tl)
  };
}

export {
  log,
  g,
  f,
  map,
}
/* No side effect */
