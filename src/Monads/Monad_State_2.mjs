// Generated by ReScript, PLEASE EDIT WITH CARE


function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function StateMonad(State) {
  let bind = (m, f) => (s => {
    let match = m(s);
    return f(match[0])(match[1]);
  });
  let $$return = a => (s => [
    a,
    s
  ]);
  let access = m => m(State.empty)[0];
  let put = s => (param => [
    undefined,
    s
  ]);
  let get = s => [
    s,
    s
  ];
  return {
    bind: bind,
    $$return: $$return,
    access: access,
    put: put,
    get: get
  };
}

function bind(m, f) {
  return s => {
    let match = m(s);
    return f(match[0])(match[1]);
  };
}

function $$return(a) {
  return s => [
    a,
    s
  ];
}

function access(m) {
  return m(0)[0];
}

function put(s) {
  return param => [
    undefined,
    s
  ];
}

function get(s) {
  return [
    s,
    s
  ];
}

let IntStateMonad = {
  bind: bind,
  $$return: $$return,
  access: access,
  put: put,
  get: get
};

function blah(s) {
  return [
    1,
    s
  ];
}

let blah2 = bind(blah, x => {
  let a = x + 1 | 0;
  return s => [
    a,
    s
  ];
});

let __x = access(blah2);

console.log(__x, "test1");

export {
  log,
  log2,
  StateMonad,
  IntStateMonad,
}
/* blah2 Not a pure module */
