// Generated by ReScript, PLEASE EDIT WITH CARE


function run(r, env) {
  return r._0(env);
}

function $$return(a) {
  return {
          TAG: "Reader",
          _0: (function (_env) {
              return a;
            })
        };
}

function ask() {
  return {
          TAG: "Reader",
          _0: (function (env) {
              return env;
            })
        };
}

function local(f, m) {
  return {
          TAG: "Reader",
          _0: (function (env) {
              return run(m, f(env));
            })
        };
}

function map(f, m) {
  return {
          TAG: "Reader",
          _0: (function (env) {
              return f(run(m, env));
            })
        };
}

function bind(f, m) {
  return {
          TAG: "Reader",
          _0: (function (env) {
              return run(f(run(m, env)), env);
            })
        };
}

function bindFlip(m, f) {
  return bind(f, m);
}

function _getState(env2) {
  return env2.state;
}

var r2 = (function (__x) {
      return bind((function (x) {
                    var __x = (function (__x) {
                          return map(_getState, __x);
                        })({
                          TAG: "Reader",
                          _0: (function (env) {
                              return env;
                            })
                        });
                    return map((function (y) {
                                  return String(y) + x;
                                }), __x);
                  }), __x);
    })({
      TAG: "Reader",
      _0: (function (_env) {
          return "danny";
        })
    });

console.log(run(r2, {
          state: 1
        }));

console.log(run(r2, {
          state: 3
        }));

export {
  run ,
  $$return ,
  ask ,
  local ,
  map ,
  bind ,
  bindFlip ,
}
/* r2 Not a pure module */
