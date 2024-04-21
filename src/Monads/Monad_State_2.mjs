// Generated by ReScript, PLEASE EDIT WITH CARE


function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function StateMonad(State) {
  var bind = function (m, f) {
    return function (s) {
      var match = m(s);
      return f(match[0])(match[1]);
    };
  };
  var $$return = function (a) {
    return function (s) {
      return [
              a,
              s
            ];
    };
  };
  var access = function (m) {
    return m(State.empty)[0];
  };
  var put = function (s) {
    return function (param) {
      return [
              undefined,
              s
            ];
    };
  };
  var get = function (s) {
    return [
            s,
            s
          ];
  };
  return {
          bind: bind,
          $$return: $$return,
          access: access,
          put: put,
          get: get
        };
}

function bind(m, f) {
  return function (s) {
    var match = m(s);
    return f(match[0])(match[1]);
  };
}

function $$return(a) {
  return function (s) {
    return [
            a,
            s
          ];
  };
}

function access(m) {
  return m(0)[0];
}

function put(s) {
  return function (param) {
    return [
            undefined,
            s
          ];
  };
}

function get(s) {
  return [
          s,
          s
        ];
}

var IntStateMonad = {
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

var blah2 = bind(blah, (function (x) {
        var a = x + 1 | 0;
        return function (s) {
          return [
                  a,
                  s
                ];
        };
      }));

((function (__x) {
        console.log(__x, "test1");
      })(access(blah2)));

export {
  log ,
  log2 ,
  StateMonad ,
  IntStateMonad ,
}
/* blah2 Not a pure module */
