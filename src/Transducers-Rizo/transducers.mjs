// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";

function compose(param, param$1) {
  var f = param$1.this;
  var g = param.this;
  return {
          this: (function (step) {
              return Curry._1(f, Curry._1(g, step));
            })
        };
}

function $great$great(g, f) {
  return compose(f, g);
}

var $less$less = compose;

function stateless(f) {
  return {
          TAG: "Reducer",
          _0: [
            undefined,
            (function (param, x, y) {
                return [
                        undefined,
                        {
                          TAG: "Continue",
                          _0: Curry._2(f, x, y)
                        }
                      ];
              })
          ]
        };
}

function transduce(param, f, r0, param$1) {
  var next = param$1._1;
  var match = Curry._1(param.this, stateless(f));
  var match$1 = match._0;
  var step = match$1[1];
  var loop = function (_s, _r, _input) {
    while(true) {
      var input = _input;
      var r = _r;
      var s = _s;
      var match = Curry._1(next, input);
      if (match === undefined) {
        return [
                s,
                r
              ];
      }
      var match$1 = Curry._3(step, s, r, match[0]);
      var r$1 = match$1[1];
      var s$1 = match$1[0];
      if (r$1.TAG !== "Continue") {
        return [
                s$1,
                r$1._0
              ];
      }
      _input = match[1];
      _r = r$1._0;
      _s = s$1;
      continue ;
    };
  };
  return loop(match$1[0], r0, param$1._0)[1];
}

function map(f) {
  var $$this = function (param) {
    var match = param._0;
    var next = match[1];
    return {
            TAG: "Reducer",
            _0: [
              match[0],
              (function (s, r, x) {
                  return Curry._3(next, s, r, Curry._1(f, x));
                })
            ]
          };
  };
  return {
          this: $$this
        };
}

function filter(p) {
  var $$this = function (param) {
    var match = param._0;
    var next = match[1];
    return {
            TAG: "Reducer",
            _0: [
              match[0],
              (function (s, r, x) {
                  if (Curry._1(p, x)) {
                    return Curry._3(next, s, r, x);
                  } else {
                    return [
                            s,
                            {
                              TAG: "Continue",
                              _0: r
                            }
                          ];
                  }
                })
            ]
          };
  };
  return {
          this: $$this
        };
}

function take(n) {
  var $$this = function (param) {
    var match = param._0;
    var next = match[1];
    return {
            TAG: "Reducer",
            _0: [
              [
                match[0],
                0
              ],
              (function (param, r, a) {
                  var i = param[1];
                  var s = param[0];
                  if (i >= n) {
                    return [
                            [
                              s,
                              i
                            ],
                            {
                              TAG: "Done",
                              _0: r
                            }
                          ];
                  }
                  var match = Curry._3(next, s, r, a);
                  return [
                          [
                            match[0],
                            i + 1 | 0
                          ],
                          match[1]
                        ];
                })
            ]
          };
  };
  return {
          this: $$this
        };
}

function iter_list(input) {
  var next = function (l) {
    if (l) {
      return [
              l.hd,
              l.tl
            ];
    }
    
  };
  return {
          TAG: "Iterator",
          _0: input,
          _1: next
        };
}

function into_list(l0, xf, iterator) {
  return List.rev(transduce(xf, (function (r, x) {
                    return {
                            hd: x,
                            tl: r
                          };
                  }), l0, iterator));
}

export {
  compose ,
  $great$great ,
  $less$less ,
  stateless ,
  transduce ,
  map ,
  filter ,
  take ,
  iter_list ,
  into_list ,
}
/* No side effect */
