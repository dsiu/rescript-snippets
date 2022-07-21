// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Char from "rescript/lib/es6/char.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Js_list from "rescript/lib/es6/js_list.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";

function log(prim) {
  console.log(prim);
  
}

function logList(l) {
  var prim = Js_list.toVector(l);
  console.log(prim);
  
}

function log2(x, y) {
  console.log(y, x);
  
}

function logList2(l, str) {
  var x = Js_list.toVector(l);
  console.log(str, x);
  
}

function lseq(n) {
  return /* Cons */{
          _0: n,
          _1: (function (param) {
              return lseq(n + 1 | 0);
            })
        };
}

function lhd(param) {
  return param._0;
}

function ltl(param) {
  return Curry._1(param._1, undefined);
}

function ltake(param, n) {
  if (n !== 0) {
    return {
            hd: param._0,
            tl: ltake(Curry._1(param._1, undefined), n - 1 | 0)
          };
  } else {
    return /* [] */0;
  }
}

function ldrop(_ll, _n) {
  while(true) {
    var n = _n;
    var ll = _ll;
    if (n === 0) {
      return ll;
    }
    _n = n - 1 | 0;
    _ll = Curry._1(ll._1, undefined);
    continue ;
  };
}

var prim = lseq(3);

console.log(prim);

var l = ltake(lseq(3), 20);

var prim$1 = Js_list.toVector(l);

console.log(prim$1);

function lmap(f, param) {
  var tf = param._1;
  return /* Cons */{
          _0: Curry._1(f, param._0),
          _1: (function (param) {
              return lmap(f, Curry._1(tf, undefined));
            })
        };
}

function lfilter(f, _param) {
  while(true) {
    var param = _param;
    var tf = param._1;
    var h = param._0;
    if (Curry._1(f, h)) {
      return /* Cons */{
              _0: h,
              _1: (function(tf){
              return function (param) {
                return lfilter(f, Curry._1(tf, undefined));
              }
              }(tf))
            };
    }
    _param = Curry._1(tf, undefined);
    continue ;
  };
}

var cubes = lfilter((function (x) {
        return x % 5 === 0;
      }), lmap((function (x) {
            return Math.imul(Math.imul(x, x), x);
          }), lseq(1)));

var l$1 = ltake(cubes, 20);

var prim$2 = Js_list.toVector(l$1);

console.log(prim$2);

function mkprimes(param) {
  var tf = param._1;
  var h = param._0;
  return /* Cons */{
          _0: h,
          _1: (function (param) {
              return mkprimes(lfilter((function (x) {
                                return Caml_int32.mod_(x, h) !== 0;
                              }), Curry._1(tf, undefined)));
            })
        };
}

var primes = mkprimes(lseq(2));

var l$2 = ltake(primes, 10);

var prim$3 = Js_list.toVector(l$2);

console.log(prim$3);

function interleave(param, l) {
  var tf = param._1;
  return /* Cons */{
          _0: param._0,
          _1: (function (param) {
              return interleave(l, Curry._1(tf, undefined));
            })
        };
}

var l$3 = ltake(interleave(lseq(20), lseq(30)), 5);

var prim$4 = Js_list.toVector(l$3);

console.log(prim$4);

function lconst(n) {
  return /* Cons */{
          _0: n,
          _1: (function (param) {
              return lconst(n);
            })
        };
}

var interleaved = interleave(lconst(0), lconst(1));

var l$4 = ltake(interleaved, 9);

var prim$5 = Js_list.toVector(l$4);

console.log(prim$5);

function allfrom(l) {
  return /* Cons */{
          _0: l,
          _1: (function (param) {
              return interleave(allfrom({
                              hd: 0,
                              tl: l
                            }), allfrom({
                              hd: 1,
                              tl: l
                            }));
            })
        };
}

var allones = allfrom(/* [] */0);

var l$5 = ltake(allones, 20);

var prim$6 = Js_list.toVector(l$5);

console.log(prim$6);

function q1_ldouble(n) {
  return /* Cons */{
          _0: n,
          _1: (function (param) {
              return q1_ldouble((n << 1));
            })
        };
}

var l$6 = ltake(q1_ldouble(2), 10);

var x = Js_list.toVector(l$6);

console.log("q1_ldouble", x);

function q2_lnth(_param, _n) {
  while(true) {
    var param = _param;
    var n = _n;
    if (n === 0) {
      return param._0;
    }
    _n = n - 1 | 0;
    _param = Curry._1(param._1, undefined);
    continue ;
  };
}

var Invalid_argument = /* @__PURE__ */Caml_exceptions.create("MO_Ch2_Being_Lazy.Invalid_argument");

function q3_lrepeating(l) {
  var lrepeating_inner = function (c, l) {
    if (c) {
      var t = c.tl;
      var x = c.hd;
      if (t) {
        return /* Cons */{
                _0: x,
                _1: (function (param) {
                    return lrepeating_inner(t, l);
                  })
              };
      } else {
        return /* Cons */{
                _0: x,
                _1: (function (param) {
                    return lrepeating_inner(l, l);
                  })
              };
      }
    }
    throw {
          RE_EXN_ID: Invalid_argument,
          _1: "empty list",
          Error: new Error()
        };
  };
  return lrepeating_inner(l, l);
}

var l$7 = ltake(q3_lrepeating({
          hd: 2,
          tl: {
            hd: 8,
            tl: {
              hd: 4,
              tl: /* [] */0
            }
          }
        }), 10);

var x$1 = Js_list.toVector(l$7);

console.log("q3_lrepeating", x$1);

function fibonacci_inner(x, y) {
  return /* Cons */{
          _0: x,
          _1: (function (param) {
              return fibonacci_inner(y, x + y | 0);
            })
        };
}

var q4_fibonacci = fibonacci_inner(0, 1);

var l$8 = ltake(q4_fibonacci, 10);

var x$2 = Js_list.toVector(l$8);

console.log("q4 fibonacci", x$2);

function q5_unleave(param) {
  var match = Curry._1(param._1, undefined);
  var t = Curry._1(match._1, undefined);
  return [
          /* Cons */{
            _0: param._0,
            _1: (function (param) {
                return q5_unleave(t)[0];
              })
          },
          /* Cons */{
            _0: match._0,
            _1: (function (param) {
                return q5_unleave(t)[1];
              })
          }
        ];
}

var match = q5_unleave(lseq(0));

var q5_b = match[1];

var q5_a = match[0];

var l$9 = ltake(q5_a, 10);

var x$3 = Js_list.toVector(l$9);

console.log("q5_unleave", x$3);

var l$10 = ltake(q5_b, 10);

var x$4 = Js_list.toVector(l$10);

console.log("q5_unleave", x$4);

function q6_letter_string(n) {
  if (n <= 26) {
    return Char.escaped(Pervasives.char_of_int(n + 64 | 0));
  } else {
    return q6_letter_string((n - 1 | 0) / 26 | 0) + q6_letter_string((n - 1 | 0) % 26 + 1 | 0);
  }
}

var alphas = lmap(q6_letter_string, lseq(1));

var l$11 = ltake(alphas, 100);

var prim$7 = Js_list.toVector(l$11);

console.log(prim$7);

var List;

export {
  List ,
  log ,
  logList ,
  log2 ,
  logList2 ,
  lseq ,
  lhd ,
  ltl ,
  ltake ,
  ldrop ,
  lmap ,
  lfilter ,
  cubes ,
  mkprimes ,
  primes ,
  interleave ,
  lconst ,
  interleaved ,
  allfrom ,
  allones ,
  q1_ldouble ,
  q2_lnth ,
  Invalid_argument ,
  q3_lrepeating ,
  q4_fibonacci ,
  q5_unleave ,
  q5_a ,
  q5_b ,
  q6_letter_string ,
  alphas ,
  
}
/* prim Not a pure module */