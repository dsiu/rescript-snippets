// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Random from "rescript/lib/es6/random.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

function last(_l) {
  while(true) {
    var l = _l;
    if (!l) {
      return ;
    }
    var rest = l.tl;
    if (!rest) {
      return Caml_option.some(l.hd);
    }
    _l = rest;
    continue ;
  };
}

function last_two(_l) {
  while(true) {
    var l = _l;
    if (!l) {
      return ;
    }
    var match = l.tl;
    if (!match) {
      return ;
    }
    var rest = match.tl;
    if (!rest) {
      return l;
    }
    _l = rest;
    continue ;
  };
}

function at(_l, _k) {
  while(true) {
    var k = _k;
    var l = _l;
    if (!l) {
      return ;
    }
    if (k === 1) {
      return Caml_option.some(l.hd);
    }
    _k = k - 1 | 0;
    _l = l.tl;
    continue ;
  };
}

function length(l) {
  if (l) {
    return 1 + length(l.tl) | 0;
  } else {
    return 0;
  }
}

function rev(l) {
  var _l$p = l;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var l$p = _l$p;
    if (!l$p) {
      return acc;
    }
    _acc = {
      hd: l$p.hd,
      tl: acc
    };
    _l$p = l$p.tl;
    continue ;
  };
}

function is_palindrome(l) {
  return Belt_List.eq(l, rev(l), (function (a, b) {
                return a === b;
              }));
}

function flatten(l) {
  var aux = function (_l$p, _acc) {
    while(true) {
      var acc = _acc;
      var l$p = _l$p;
      if (!l$p) {
        return acc;
      }
      var x = l$p.hd;
      if (x.TAG === /* One */0) {
        _acc = {
          hd: x._0,
          tl: acc
        };
        _l$p = l$p.tl;
        continue ;
      }
      _acc = aux(x._0, acc);
      _l$p = l$p.tl;
      continue ;
    };
  };
  return rev(aux(l, /* [] */0));
}

function compress(_l) {
  while(true) {
    var l = _l;
    if (!l) {
      return l;
    }
    var match = l.tl;
    if (!match) {
      return l;
    }
    var t = match.tl;
    var b = match.hd;
    var a = l.hd;
    if (!Caml_obj.caml_equal(a, b)) {
      return {
              hd: a,
              tl: compress({
                    hd: b,
                    tl: t
                  })
            };
    }
    _l = {
      hd: b,
      tl: t
    };
    continue ;
  };
}

function pack(l) {
  var aux = function (_l, _current, _acc) {
    while(true) {
      var acc = _acc;
      var current = _current;
      var l = _l;
      if (!l) {
        return /* [] */0;
      }
      var match = l.tl;
      var x = l.hd;
      if (!match) {
        return {
                hd: {
                  hd: x,
                  tl: current
                },
                tl: acc
              };
      }
      var t = match.tl;
      var b = match.hd;
      if (Caml_obj.caml_equal(x, b)) {
        _current = {
          hd: x,
          tl: current
        };
        _l = {
          hd: b,
          tl: t
        };
        continue ;
      }
      _acc = {
        hd: {
          hd: x,
          tl: current
        },
        tl: acc
      };
      _current = /* [] */0;
      _l = {
        hd: b,
        tl: t
      };
      continue ;
    };
  };
  return rev(aux(l, /* [] */0, /* [] */0));
}

function encode(l) {
  var aux = function (_l, _count, _acc) {
    while(true) {
      var acc = _acc;
      var count = _count;
      var l = _l;
      if (!l) {
        return /* [] */0;
      }
      var match = l.tl;
      var x = l.hd;
      if (!match) {
        return {
                hd: [
                  count + 1 | 0,
                  x
                ],
                tl: acc
              };
      }
      var t = match.tl;
      var b = match.hd;
      if (Caml_obj.caml_equal(x, b)) {
        _count = count + 1 | 0;
        _l = {
          hd: b,
          tl: t
        };
        continue ;
      }
      _acc = {
        hd: [
          count + 1 | 0,
          x
        ],
        tl: acc
      };
      _count = 0;
      _l = {
        hd: b,
        tl: t
      };
      continue ;
    };
  };
  return rev(aux(l, 0, /* [] */0));
}

function encode_11(l) {
  var create_tuple = function (cnt, elem) {
    if (cnt === 1) {
      return {
              TAG: /* One */0,
              _0: elem
            };
    } else {
      return {
              TAG: /* Many */1,
              _0: cnt,
              _1: elem
            };
    }
  };
  var aux = function (_l, _count, _acc) {
    while(true) {
      var acc = _acc;
      var count = _count;
      var l = _l;
      if (!l) {
        return /* [] */0;
      }
      var match = l.tl;
      var x = l.hd;
      if (!match) {
        return {
                hd: create_tuple(count + 1 | 0, x),
                tl: acc
              };
      }
      var t = match.tl;
      var b = match.hd;
      if (Caml_obj.caml_equal(x, b)) {
        _count = count + 1 | 0;
        _l = {
          hd: b,
          tl: t
        };
        continue ;
      }
      _acc = {
        hd: create_tuple(count + 1 | 0, x),
        tl: acc
      };
      _count = 0;
      _l = {
        hd: b,
        tl: t
      };
      continue ;
    };
  };
  return rev(aux(l, 0, /* [] */0));
}

function decode(l) {
  var many = function (_acc, _n, x) {
    while(true) {
      var n = _n;
      var acc = _acc;
      if (n === 0) {
        return acc;
      }
      _n = n - 1 | 0;
      _acc = {
        hd: x,
        tl: acc
      };
      continue ;
    };
  };
  var _l = rev(l);
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var l$1 = _l;
    if (!l$1) {
      return acc;
    }
    var x = l$1.hd;
    if (x.TAG === /* One */0) {
      _acc = {
        hd: x._0,
        tl: acc
      };
      _l = l$1.tl;
      continue ;
    }
    _acc = many(acc, x._0, x._1);
    _l = l$1.tl;
    continue ;
  };
}

function encode_13(l) {
  var rle = function (count, x) {
    if (count !== 0) {
      return {
              TAG: /* Many */1,
              _0: count + 1 | 0,
              _1: x
            };
    } else {
      return {
              TAG: /* One */0,
              _0: x
            };
    }
  };
  var aux = function (_l, _count, _acc) {
    while(true) {
      var acc = _acc;
      var count = _count;
      var l = _l;
      if (!l) {
        return /* [] */0;
      }
      var match = l.tl;
      var x = l.hd;
      if (!match) {
        return {
                hd: rle(count, x),
                tl: acc
              };
      }
      var t = match.tl;
      var b = match.hd;
      if (Caml_obj.caml_equal(x, b)) {
        _count = count + 1 | 0;
        _l = {
          hd: b,
          tl: t
        };
        continue ;
      }
      _acc = {
        hd: rle(count, x),
        tl: acc
      };
      _count = 0;
      _l = {
        hd: b,
        tl: t
      };
      continue ;
    };
  };
  return rev(aux(l, 0, /* [] */0));
}

function duplicate(l) {
  if (!l) {
    return /* [] */0;
  }
  var h = l.hd;
  return {
          hd: h,
          tl: {
            hd: h,
            tl: duplicate(l.tl)
          }
        };
}

function replicate(l, n) {
  var prepend = function (x, _n, _acc) {
    while(true) {
      var acc = _acc;
      var n = _n;
      if (n === 0) {
        return acc;
      }
      _acc = {
        hd: x,
        tl: acc
      };
      _n = n - 1 | 0;
      continue ;
    };
  };
  var _l = rev(l);
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var l$1 = _l;
    if (!l$1) {
      return acc;
    }
    _acc = prepend(l$1.hd, n, acc);
    _l = l$1.tl;
    continue ;
  };
}

function drop(l, n) {
  var aux = function (_l, _i) {
    while(true) {
      var i = _i;
      var l = _l;
      if (!l) {
        return /* [] */0;
      }
      var t = l.tl;
      if (i !== n) {
        return {
                hd: l.hd,
                tl: aux(t, i + 1 | 0)
              };
      }
      _i = 1;
      _l = t;
      continue ;
    };
  };
  return aux(l, 1);
}

function split(l, n) {
  var _l = l;
  var _i = n;
  var _acc = /* [] */0;
  while(true) {
    var acc = _acc;
    var i = _i;
    var l$1 = _l;
    if (!l$1) {
      return [
              rev(acc),
              /* [] */0
            ];
    }
    if (i === 0) {
      return [
              rev(acc),
              l$1
            ];
    }
    _acc = {
      hd: l$1.hd,
      tl: acc
    };
    _i = i - 1 | 0;
    _l = l$1.tl;
    continue ;
  };
}

function slice(l, i, k) {
  var take = function (l, n) {
    if (l && n !== 0) {
      return {
              hd: l.hd,
              tl: take(l.tl, n - 1 | 0)
            };
    } else {
      return /* [] */0;
    }
  };
  var drop = function (_l, _n) {
    while(true) {
      var n = _n;
      var l = _l;
      if (!l) {
        return /* [] */0;
      }
      if (n === 0) {
        return l;
      }
      _n = n - 1 | 0;
      _l = l.tl;
      continue ;
    };
  };
  return take(drop(l, i), (k - i | 0) + 1 | 0);
}

function rotate(l, n) {
  var len = length(l);
  var n$1 = len === 0 ? 0 : Caml_int32.mod_(Caml_int32.mod_(n, len) + len | 0, len);
  if (n$1 === 0) {
    return l;
  }
  var match = split(l, n$1);
  return Belt_List.concat(match[1], match[0]);
}

function remove_at(l, n) {
  if (!l) {
    return /* [] */0;
  }
  var t = l.tl;
  if (n === 0) {
    return t;
  } else {
    return {
            hd: l.hd,
            tl: remove_at(t, n - 1 | 0)
          };
  }
}

function insert_at(l, n, x) {
  if (!l) {
    return {
            hd: x,
            tl: /* [] */0
          };
  }
  var t = l.tl;
  var h = l.hd;
  if (n === 0) {
    return {
            hd: x,
            tl: {
              hd: h,
              tl: t
            }
          };
  } else {
    return {
            hd: h,
            tl: insert_at(t, n - 1 | 0, x)
          };
  }
}

function range(a, b) {
  var aux = function (a, b) {
    if (a > b) {
      return /* [] */0;
    } else {
      return {
              hd: a,
              tl: aux(a + 1 | 0, b)
            };
    }
  };
  if (a > b) {
    return aux(b, a);
  } else {
    return aux(a, b);
  }
}

function range_tail_recur(a, b) {
  var aux = function (_acc, _high, low) {
    while(true) {
      var high = _high;
      var acc = _acc;
      if (high < low) {
        return acc;
      }
      _high = high - 1 | 0;
      _acc = {
        hd: high,
        tl: acc
      };
      continue ;
    };
  };
  if (b > a) {
    return aux(/* [] */0, b, a);
  } else {
    return aux(/* [] */0, a, b);
  }
}

function rand_select(list, n) {
  var extract_rand = function (list, len) {
    var _l = list;
    var _acc = /* [] */0;
    var _n = Random.$$int(len);
    while(true) {
      var n = _n;
      var acc = _acc;
      var l = _l;
      if (l) {
        var t = l.tl;
        var h = l.hd;
        if (n === 0) {
          return [
                  h,
                  Belt_List.concat(acc, t)
                ];
        }
        _n = n - 1 | 0;
        _acc = {
          hd: h,
          tl: acc
        };
        _l = t;
        continue ;
      }
      throw {
            RE_EXN_ID: "Not_found",
            Error: new Error()
          };
    };
  };
  var len = length(list);
  var _n = n < len ? n : len;
  var _acc = /* [] */0;
  var _list = list;
  var _len = len;
  while(true) {
    var len$1 = _len;
    var list$1 = _list;
    var acc = _acc;
    var n$1 = _n;
    if (n$1 === 0) {
      return acc;
    }
    var match = extract_rand(list$1, len$1);
    _len = len$1 - 1 | 0;
    _list = match[1];
    _acc = {
      hd: match[0],
      tl: acc
    };
    _n = n$1 - 1 | 0;
    continue ;
  };
}

export {
  last ,
  last_two ,
  at ,
  length ,
  rev ,
  is_palindrome ,
  flatten ,
  compress ,
  pack ,
  encode ,
  encode_11 ,
  decode ,
  encode_13 ,
  duplicate ,
  replicate ,
  drop ,
  split ,
  slice ,
  rotate ,
  remove_at ,
  insert_at ,
  range ,
  range_tail_recur ,
  rand_select ,
  
}
/* No side effect */