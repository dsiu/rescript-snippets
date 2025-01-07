// Generated by ReScript, PLEASE EDIT WITH CARE


function run(r, env) {
  return r._0(env);
}

function $$return(a) {
  return {
    TAG: "Reader",
    _0: _env => a
  };
}

function ask() {
  return {
    TAG: "Reader",
    _0: env => env
  };
}

function local(f, m) {
  return {
    TAG: "Reader",
    _0: env => run(m, f(env))
  };
}

function map(f, m) {
  return {
    TAG: "Reader",
    _0: env => f(run(m, env))
  };
}

function bind(f, m) {
  return {
    TAG: "Reader",
    _0: env => run(f(run(m, env)), env)
  };
}

function bindFlip(m, f) {
  return bind(f, m);
}

function _getState(env2) {
  return env2.state;
}

let __x = {
  TAG: "Reader",
  _0: _env => "danny"
};

let r2 = bind(x => {
  let __x = map(_getState, {
    TAG: "Reader",
    _0: env => env
  });
  return map(y => String(y) + x, __x);
}, __x);

console.log(run(r2, {
  state: 1
}));

console.log(run(r2, {
  state: 3
}));

export {
  run,
  $$return,
  ask,
  local,
  map,
  bind,
  bindFlip,
}
/* r2 Not a pure module */
