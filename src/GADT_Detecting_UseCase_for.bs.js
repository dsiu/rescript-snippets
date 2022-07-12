// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Caml_obj from "../node_modules/rescript/lib/es6/caml_obj.js";
import * as Pervasives from "../node_modules/rescript/lib/es6/pervasives.js";

function $$eval(_expr) {
  while(true) {
    var expr = _expr;
    switch (expr.TAG | 0) {
      case /* Value */0 :
          return expr._0;
      case /* If */1 :
          var match = $$eval(expr._0);
          if (match.TAG !== /* Bool */0) {
            return Pervasives.failwith("Invalid AST");
          }
          if (match._0) {
            _expr = expr._1;
            continue ;
          }
          _expr = expr._2;
          continue ;
      case /* Eq */2 :
          var match$1 = $$eval(expr._0);
          var match$2 = $$eval(expr._1);
          if (match$1.TAG === /* Bool */0 || match$2.TAG === /* Bool */0) {
            return Pervasives.failwith("Invalid AST");
          } else {
            return {
                    TAG: /* Bool */0,
                    _0: match$1._0 === match$2._0
                  };
          }
      case /* Lt */3 :
          var match$3 = $$eval(expr._0);
          var match$4 = $$eval(expr._1);
          if (match$3.TAG === /* Bool */0 || match$4.TAG === /* Bool */0) {
            return Pervasives.failwith("Invalid AST");
          } else {
            return {
                    TAG: /* Bool */0,
                    _0: match$3._0 < match$4._0
                  };
          }
      
    }
  };
}

function eval_int(value) {
  if (value.TAG === /* Bool */0) {
    return Pervasives.failwith("Got Bool, expected Int");
  } else {
    return value._0;
  }
}

function eval_bool(value) {
  if (value.TAG === /* Bool */0) {
    return value._0;
  } else {
    return Pervasives.failwith("Got Int, expected Bool");
  }
}

var ADTs = {
  $$eval: $$eval,
  eval_int: eval_int,
  eval_bool: eval_bool
};

console.log($$eval({
          TAG: /* If */1,
          _0: {
            TAG: /* Lt */3,
            _0: {
              TAG: /* Value */0,
              _0: {
                TAG: /* Int */1,
                _0: 2
              }
            },
            _1: {
              TAG: /* Value */0,
              _0: {
                TAG: /* Int */1,
                _0: 4
              }
            }
          },
          _1: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */1,
              _0: 42
            }
          },
          _2: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */1,
              _0: 0
            }
          }
        }));

function $$eval$1(_expr) {
  while(true) {
    var expr = _expr;
    switch (expr.TAG | 0) {
      case /* Value */0 :
          return expr._0._0;
      case /* If */1 :
          if ($$eval$1(expr._0)) {
            _expr = expr._1;
            continue ;
          }
          _expr = expr._2;
          continue ;
      case /* Eq */2 :
          return Caml_obj.caml_equal($$eval$1(expr._0), $$eval$1(expr._1));
      case /* Lt */3 :
          return $$eval$1(expr._0) < $$eval$1(expr._1);
      
    }
  };
}

var GADT = {
  $$eval: $$eval$1
};

console.log($$eval$1({
          TAG: /* If */1,
          _0: {
            TAG: /* Eq */2,
            _0: {
              TAG: /* Value */0,
              _0: {
                TAG: /* Int */1,
                _0: 2
              }
            },
            _1: {
              TAG: /* Value */0,
              _0: {
                TAG: /* Int */1,
                _0: 2
              }
            }
          },
          _1: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */1,
              _0: 42
            }
          },
          _2: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Int */1,
              _0: 12
            }
          }
        }));

console.log($$eval$1({
          TAG: /* If */1,
          _0: {
            TAG: /* Eq */2,
            _0: {
              TAG: /* Value */0,
              _0: {
                TAG: /* Int */1,
                _0: 2
              }
            },
            _1: {
              TAG: /* Value */0,
              _0: {
                TAG: /* Int */1,
                _0: 2
              }
            }
          },
          _1: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Bool */0,
              _0: true
            }
          },
          _2: {
            TAG: /* Value */0,
            _0: {
              TAG: /* Bool */0,
              _0: false
            }
          }
        }));

export {
  ADTs ,
  GADT ,
  
}
/*  Not a pure module */
