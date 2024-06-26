// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Relude_StateT from "relude/src/Relude_StateT.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var State = Relude_StateT.State.WithState({});

var map = State.Infix.$less$$great;

var voidLeft = State.Infix.$$great;

var flipMap = State.Infix.$less$$$great;

var applySecond = State.Infix.$star$great;

var bind = State.Infix.$great$great$eq;

function push(x) {
  return voidLeft(Relude_StateT.State.modify(function (xs) {
                  return {
                          hd: x,
                          tl: xs
                        };
                }), x);
}

var pop = bind(State.get, (function (values) {
        if (values) {
          return voidLeft(State.put(values.tl), values.hd);
        } else {
          return voidLeft(State.put(/* [] */0), undefined);
        }
      }));

var Stack = {
  push: push,
  pop: pop
};

var result = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(State.pure(2));

((function (__x) {
        console.log("pure", __x);
      })(result));

var result$1 = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(bind(State.pure(2), (function (a) {
            return voidLeft(State.put({
                            hd: a,
                            tl: /* [] */0
                          }), a);
          })));

((function (__x) {
        console.log("put", __x);
      })(result$1));

var result$2 = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(bind(push(1), (function (param) {
            return push(2);
          })));

((function (__x) {
        console.log("stack example 1 (push)", __x);
      })(result$2));

var result$3 = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(bind(push(1), (function (param) {
            return bind(push(2), (function (param) {
                          return bind(push(3), (function (param) {
                                        return pop;
                                      }));
                        }));
          })));

((function (__x) {
        console.log("stack example 2 (push, pop)", __x);
      })(result$3));

var result$4 = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(bind(push(1), (function (param) {
            return bind(push(1), (function (param) {
                          return bind(push(2), (function (param) {
                                        return bind(push(3), (function (param) {
                                                      return bind(pop, (function (param) {
                                                                    return bind(pop, (function (param) {
                                                                                  return bind(pop, (function (param) {
                                                                                                return bind(push(4), (function (param) {
                                                                                                              return push(5);
                                                                                                            }));
                                                                                              }));
                                                                                }));
                                                                  }));
                                                    }));
                                      }));
                        }));
          })));

((function (__x) {
        console.log("stack example 3", __x);
      })(result$4[0]));

((function (__x) {
        console.log("stack example 3", __x);
      })(result$4[1]));

var result$5 = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(bind(push(1), (function (param) {
            return bind(push(1), (function (param) {
                          return bind(push(2), (function (param) {
                                        return bind(push(3), (function (param) {
                                                      return bind(pop, (function (param) {
                                                                    return bind(pop, (function (param) {
                                                                                  return bind(pop, (function (param) {
                                                                                                return bind(push(4), (function (param) {
                                                                                                              return flipMap(push(5), (function (a) {
                                                                                                                            return Math.imul(a, 100);
                                                                                                                          }));
                                                                                                            }));
                                                                                              }));
                                                                                }));
                                                                  }));
                                                    }));
                                      }));
                        }));
          })));

((function (__x) {
        console.log("stack example 4", __x);
      })(result$5[0]));

((function (__x) {
        console.log("stack example 4", __x);
      })(result$5[1]));

var result$6 = (function (__x) {
      return State.runStateT(/* [] */0, __x);
    })(applySecond(applySecond(push(1), push(2)), push(3)));

var s = result$6[1];

var a = result$6[0];

((function (__x) {
        console.log("*> loses state", __x);
      })(a));

((function (__x) {
        console.log("*> loses state", __x);
      })(s));

var StateT;

export {
  log ,
  log2 ,
  StateT ,
  State ,
  map ,
  voidLeft ,
  flipMap ,
  applySecond ,
  bind ,
  Stack ,
  result$6 as result,
  a ,
  s ,
}
/* State Not a pure module */
