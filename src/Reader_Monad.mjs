// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Array from "rescript/lib/es6/array.js";
import * as Curry from "rescript/lib/es6/curry.js";

var r = /* Reader */{
  _0: (function (e) {
      return e + 1 | 0;
    })
};

function run(r, env) {
  return Curry._1(r._0, env);
}

var r1 = /* Reader */{
  _0: (function (e) {
      return e + 1 | 0;
    })
};

run(r1, 1);

function $$return(a) {
  return /* Reader */{
          _0: (function (_env) {
              return a;
            })
        };
}

var r2 = /* Reader */{
  _0: (function (_env) {
      return 99;
    })
};

run(r2, 1);

function ask(param) {
  return /* Reader */{
          _0: (function (env) {
              return env;
            })
        };
}

var r3 = /* Reader */{
  _0: (function (env) {
      return env;
    })
};

run(r3, 123);

function local(f, m) {
  return /* Reader */{
          _0: (function (env) {
              return run(m, Curry._1(f, env));
            })
        };
}

var r4 = /* Reader */{
  _0: (function (e) {
      return e + 1 | 0;
    })
};

var r5 = local((function (e) {
        return -e | 0;
      }), r4);

run(r5, 1);

function map(f, m) {
  return /* Reader */{
          _0: (function (env) {
              return Curry._1(f, run(m, env));
            })
        };
}

var r6 = /* Reader */{
  _0: (function (e) {
      return e + 1 | 0;
    })
};

var r7 = map((function (x) {
        return Math.imul(x, 10);
      }), r6);

run(r7, 1);

function bind(f, m) {
  return /* Reader */{
          _0: (function (env) {
              return run(Curry._1(f, run(m, env)), env);
            })
        };
}

var r8 = /* Reader */{
  _0: (function (e) {
      return e + 1 | 0;
    })
};

var r9 = bind((function (x) {
        return /* Reader */{
                _0: (function (param) {
                    return (x << 1);
                  })
              };
      }), r8);

run(r9, 1);

var r10 = /* Reader */{
  _0: (function (e) {
      return e + 1 | 0;
    })
};

var r11 = bind((function (x) {
        var a = (x << 1);
        return /* Reader */{
                _0: (function (_env) {
                    return a;
                  })
              };
      }), r10);

run(r11, 1);

function greet(name, greeting) {
  return greeting + ": " + name;
}

function lines(param) {
  return $$Array.map((function (prim) {
                console.log(prim);
                
              }), param);
}

var ra = /* Reader */{
  _0: (function (param) {
      return greet("One", param);
    })
};

var rb = /* Reader */{
  _0: (function (param) {
      return greet("Two", param);
    })
};

var rc = /* Reader */{
  _0: (function (param) {
      return greet("Three", param);
    })
};

var r12 = bind((function (a) {
        return bind((function (b) {
                      return bind((function (c) {
                                    var a$1 = lines([
                                          a,
                                          b,
                                          c
                                        ]);
                                    return /* Reader */{
                                            _0: (function (_env) {
                                                return a$1;
                                              })
                                          };
                                  }), rc);
                    }), rb);
      }), ra);

run(r12, "Hello");

function bindFlip(m, f) {
  return bind(f, m);
}

function greet$1(name, greeting) {
  return greeting + ": " + name;
}

function lines$1(param) {
  return $$Array.map((function (prim) {
                console.log(prim);
                
              }), param);
}

var ra$1 = /* Reader */{
  _0: (function (param) {
      return greet$1("One", param);
    })
};

var rb$1 = /* Reader */{
  _0: (function (param) {
      return greet$1("Two", param);
    })
};

var rc$1 = /* Reader */{
  _0: (function (param) {
      return greet$1("Three", param);
    })
};

var r13 = bind((function (a) {
        return bind((function (b) {
                      return bind((function (c) {
                                    var a$1 = lines$1([
                                          a,
                                          b,
                                          c
                                        ]);
                                    return /* Reader */{
                                            _0: (function (_env) {
                                                return a$1;
                                              })
                                          };
                                  }), rc$1);
                    }), rb$1);
      }), ra$1);

run(r13, "Goodbye");

export {
  r ,
  run ,
  r1 ,
  $$return ,
  r2 ,
  ask ,
  r3 ,
  local ,
  r4 ,
  r5 ,
  map ,
  r6 ,
  r7 ,
  bind ,
  r8 ,
  r9 ,
  r10 ,
  r11 ,
  r12 ,
  bindFlip ,
  greet$1 as greet,
  lines$1 as lines,
  ra$1 as ra,
  rb$1 as rb,
  rc$1 as rc,
  r13 ,
  
}
/*  Not a pure module */
