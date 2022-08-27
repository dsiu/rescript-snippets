// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

var Ill_typed = /* @__PURE__ */Caml_exceptions.create("Ch9_GADTs.Ill_typed");

function $$eval(_expr) {
  while(true) {
    var expr = _expr;
    switch (expr.TAG | 0) {
      case /* Value */0 :
          return expr._0;
      case /* Eq */1 :
          var match = $$eval(expr._0);
          var match$1 = $$eval(expr._1);
          if (match.TAG === /* Int */0) {
            if (match$1.TAG === /* Int */0) {
              return {
                      TAG: /* Bool */1,
                      _0: match._0 === match$1._0
                    };
            }
            throw {
                  RE_EXN_ID: Ill_typed,
                  Error: new Error()
                };
          }
          throw {
                RE_EXN_ID: Ill_typed,
                Error: new Error()
              };
      case /* Plus */2 :
          var match$2 = $$eval(expr._0);
          var match$3 = $$eval(expr._1);
          if (match$2.TAG === /* Int */0) {
            if (match$3.TAG === /* Int */0) {
              return {
                      TAG: /* Int */0,
                      _0: match$2._0 + match$3._0 | 0
                    };
            }
            throw {
                  RE_EXN_ID: Ill_typed,
                  Error: new Error()
                };
          }
          throw {
                RE_EXN_ID: Ill_typed,
                Error: new Error()
              };
      case /* If */3 :
          var b = $$eval(expr._0);
          if (b.TAG === /* Int */0) {
            throw {
                  RE_EXN_ID: Ill_typed,
                  Error: new Error()
                };
          }
          if (b._0) {
            _expr = expr._1;
            continue ;
          }
          _expr = expr._2;
          continue ;
      
    }
  };
}

console.log($$eval({
          TAG: /* Plus */2,
          _0: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */0,
              _0: 3
            }
          },
          _1: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */0,
              _0: 2
            }
          }
        }));

console.log($$eval({
          TAG: /* If */3,
          _0: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Bool */1,
              _0: true
            }
          },
          _1: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */0,
              _0: 4
            }
          },
          _2: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */0,
              _0: 10
            }
          }
        }));

function $$int(x) {
  return {
          TAG: /* Value */0,
          _0: {
            TAG: /* Int */0,
            _0: x
          }
        };
}

function bool(x) {
  return {
          TAG: /* Value */0,
          _0: {
            TAG: /* Bool */1,
            _0: x
          }
        };
}

function if_(c, t, e) {
  return {
          TAG: /* If */3,
          _0: c,
          _1: t,
          _2: e
        };
}

function eq(x, y) {
  return {
          TAG: /* Eq */1,
          _0: x,
          _1: y
        };
}

function plus(x, y) {
  return {
          TAG: /* Plus */2,
          _0: x,
          _1: y
        };
}

function int_eval(expr) {
  var x = $$eval(expr);
  if (x.TAG === /* Int */0) {
    return x._0;
  }
  throw {
        RE_EXN_ID: Ill_typed,
        Error: new Error()
      };
}

function bool_eval(expr) {
  var x = $$eval(expr);
  if (x.TAG !== /* Int */0) {
    return x._0;
  }
  throw {
        RE_EXN_ID: Ill_typed,
        Error: new Error()
      };
}

var Typesafe_lang = {
  $$int: $$int,
  bool: bool,
  if_: if_,
  eq: eq,
  plus: plus,
  int_eval: int_eval,
  bool_eval: bool_eval
};

console.log({
      TAG: /* Plus */2,
      _0: {
        TAG: /* Value */0,
        _0: {
          TAG: /* Int */0,
          _0: 3
        }
      },
      _1: {
        TAG: /* Value */0,
        _0: {
          TAG: /* Int */0,
          _0: 2
        }
      }
    });

console.log("Try_Ordinary_Variants");

function i(x) {
  return {
          TAG: /* Value */0,
          _0: {
            TAG: /* Int */0,
            _0: x
          }
        };
}

function b(x) {
  return {
          TAG: /* Value */0,
          _0: {
            TAG: /* Bool */1,
            _0: x
          }
        };
}

function $plus(x, y) {
  return {
          TAG: /* Plus */2,
          _0: x,
          _1: y
        };
}

console.log({
      TAG: /* Value */0,
      _0: {
        TAG: /* Int */0,
        _0: 3
      }
    });

console.log({
      TAG: /* Value */0,
      _0: {
        TAG: /* Bool */1,
        _0: false
      }
    });

console.log({
      TAG: /* Plus */2,
      _0: {
        TAG: /* Value */0,
        _0: {
          TAG: /* Int */0,
          _0: 3
        }
      },
      _1: {
        TAG: /* Value */0,
        _0: {
          TAG: /* Int */0,
          _0: 2
        }
      }
    });

console.log({
      TAG: /* Value */0,
      _0: {
        TAG: /* Bool */1,
        _0: 3
      }
    });

var Try_Ordinary_Variants = {
  i: i,
  b: b,
  $plus: $plus
};

console.log("GADTs");

console.log({
      TAG: /* Value */0,
      _0: {
        TAG: /* Int */0,
        _0: 3
      }
    });

console.log({
      TAG: /* Plus */2,
      _0: {
        TAG: /* Value */0,
        _0: {
          TAG: /* Int */0,
          _0: 3
        }
      },
      _1: {
        TAG: /* Value */0,
        _0: {
          TAG: /* Int */0,
          _0: 6
        }
      }
    });

function eval_value(v) {
  return v._0;
}

function $$eval$1(_e) {
  while(true) {
    var e = _e;
    switch (e.TAG | 0) {
      case /* Value */0 :
          return e._0._0;
      case /* Eq */1 :
          return $$eval$1(e._0) === $$eval$1(e._1);
      case /* Plus */2 :
          return $$eval$1(e._0) + $$eval$1(e._1) | 0;
      case /* If */3 :
          if ($$eval$1(e._0)) {
            _e = e._1;
            continue ;
          }
          _e = e._2;
          continue ;
      
    }
  };
}

function eval_value_2(v) {
  return v._0;
}

function eval_2(_e) {
  while(true) {
    var e = _e;
    switch (e.TAG | 0) {
      case /* Value */0 :
          return e._0._0;
      case /* Eq */1 :
          return eval_2(e._0) === eval_2(e._1);
      case /* Plus */2 :
          return eval_2(e._0) + eval_2(e._1) | 0;
      case /* If */3 :
          if (eval_2(e._0)) {
            _e = e._1;
            continue ;
          }
          _e = e._2;
          continue ;
      
    }
  };
}

var GADTs = {
  eval_value: eval_value,
  $$eval: $$eval$1,
  eval_value_2: eval_value_2,
  eval_2: eval_2
};

export {
  Ill_typed ,
  $$eval ,
  Typesafe_lang ,
  Try_Ordinary_Variants ,
  GADTs ,
}
/*  Not a pure module */
