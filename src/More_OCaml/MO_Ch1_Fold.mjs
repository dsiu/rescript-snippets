// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Js_int from "rescript/lib/es6/js_int.js";
import * as Js_list from "rescript/lib/es6/js_list.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";

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

function fold_left(f, _a, _l) {
  while(true) {
    var l = _l;
    var a = _a;
    if (!l) {
      return a;
    }
    _l = l.tl;
    _a = Curry._2(f, a, l.hd);
    continue ;
  };
}

var prim = fold_left((function (prim0, prim1) {
        return prim0 + prim1 | 0;
      }), 0, {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    });

console.log(prim);

function fold_right(f, l, a) {
  if (l) {
    return Curry._2(f, l.hd, fold_right(f, l.tl, a));
  } else {
    return a;
  }
}

var prim$1 = fold_right((function (prim0, prim1) {
        return prim0 + prim1 | 0;
      }), {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, 0);

console.log(prim$1);

function max(a, b) {
  if (Caml_obj.greaterthan(a, b)) {
    return a;
  } else {
    return b;
  }
}

var prim$2 = fold_left((function (prim0, prim1) {
        return Math.max(prim0, prim1);
      }), Js_int.min, {
      hd: 2,
      tl: {
        hd: 4,
        tl: {
          hd: 6,
          tl: {
            hd: 20,
            tl: {
              hd: 1,
              tl: /* [] */0
            }
          }
        }
      }
    });

console.log(prim$2);

function all(l) {
  return fold_left((function (prim0, prim1) {
                if (prim0) {
                  return prim1;
                } else {
                  return false;
                }
              }), true, l);
}

function any(l) {
  return fold_left((function (prim0, prim1) {
                if (prim0) {
                  return true;
                } else {
                  return prim1;
                }
              }), false, l);
}

function map(f, l) {
  return fold_right((function (e, a) {
                return {
                        hd: Curry._1(f, e),
                        tl: a
                      };
              }), l, /* [] */0);
}

var l = map((function (x) {
        return (x << 1);
      }), {
      hd: 2,
      tl: {
        hd: 9,
        tl: {
          hd: 1,
          tl: /* [] */0
        }
      }
    });

var x = Js_list.toVector(l);

console.log("map", x);

function fold_right_tr(f, l, e) {
  return fold_left((function (x, y) {
                return Curry._2(f, y, x);
              }), e, Js_list.rev(l));
}

function copy(l) {
  return fold_right((function (e, a) {
                return {
                        hd: e,
                        tl: a
                      };
              }), l, /* [] */0);
}

var l$1 = copy({
      hd: 2,
      tl: {
        hd: 5,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    });

var x$1 = Js_list.toVector(l$1);

console.log("copy", x$1);

function copy_l(l) {
  return fold_left((function (e, a) {
                return {
                        hd: a,
                        tl: e
                      };
              }), /* [] */0, Js_list.rev(l));
}

var l$2 = copy_l({
      hd: 2,
      tl: {
        hd: 5,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    });

var x$2 = Js_list.toVector(l$2);

console.log("copy_l", x$2);

function append(x, y) {
  return fold_right((function (e, a) {
                return {
                        hd: e,
                        tl: a
                      };
              }), x, y);
}

var l$3 = append({
      hd: 8,
      tl: {
        hd: 1,
        tl: {
          hd: 2,
          tl: /* [] */0
        }
      }
    }, {
      hd: 9,
      tl: {
        hd: 6,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    });

var x$3 = Js_list.toVector(l$3);

console.log("append", x$3);

function split(l) {
  return fold_right((function (param, param$1) {
                return [
                        {
                          hd: param[0],
                          tl: param$1[0]
                        },
                        {
                          hd: param[1],
                          tl: param$1[1]
                        }
                      ];
              }), l, [
              /* [] */0,
              /* [] */0
            ]);
}

var prim$3 = split({
      hd: [
        1,
        "one"
      ],
      tl: {
        hd: [
          2,
          "two"
        ],
        tl: /* [] */0
      }
    });

console.log(prim$3);

function fold_tree(f, e, t) {
  if (t) {
    return Curry._3(f, t._0, fold_tree(f, e, t._1), fold_tree(f, e, t._2));
  } else {
    return e;
  }
}

function tree_size(t) {
  return fold_tree((function (param, l, r) {
                return (1 + l | 0) + r | 0;
              }), 0, t);
}

function tree_sum(t) {
  return fold_tree((function (x, l, r) {
                return (x + l | 0) + r | 0;
              }), 0, t);
}

var exp_tr = /* Br */{
  _0: 1,
  _1: /* Br */{
    _0: 0,
    _1: /* Lf */0,
    _2: /* Lf */0
  },
  _2: /* Br */{
    _0: 6,
    _1: /* Br */{
      _0: 4,
      _1: /* Lf */0,
      _2: /* Lf */0
    },
    _2: /* Lf */0
  }
};

function tree_preorder(t) {
  return fold_tree((function (x, l, r) {
                return append(append({
                                hd: x,
                                tl: /* [] */0
                              }, l), r);
              }), /* [] */0, t);
}

function tree_inorder(t) {
  return fold_tree((function (x, l, r) {
                return append(append(l, {
                                hd: x,
                                tl: /* [] */0
                              }), r);
              }), /* [] */0, t);
}

function tree_postorder(t) {
  return fold_tree((function (x, l, r) {
                return append(append(l, r), {
                            hd: x,
                            tl: /* [] */0
                          });
              }), /* [] */0, t);
}

var l$4 = tree_preorder(exp_tr);

var x$4 = Js_list.toVector(l$4);

console.log("preorder", x$4);

var l$5 = tree_inorder(exp_tr);

var x$5 = Js_list.toVector(l$5);

console.log("inorder", x$5);

var l$6 = tree_postorder(exp_tr);

var x$6 = Js_list.toVector(l$6);

console.log("postorder", x$6);

function q1_deduct(exp, budget) {
  return fold_left((function (prim0, prim1) {
                return prim0 - prim1 | 0;
              }), budget, exp);
}

var x$7 = q1_deduct({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, 10);

console.log("q1_deduct", x$7);

function q2_length(l) {
  return fold_right((function (param, a) {
                return a + 1 | 0;
              }), l, 0);
}

var x$8 = q2_length({
      hd: 4,
      tl: {
        hd: 6,
        tl: {
          hd: 1,
          tl: {
            hd: 6,
            tl: {
              hd: 4,
              tl: {
                hd: 9,
                tl: {
                  hd: 2,
                  tl: {
                    hd: 1,
                    tl: /* [] */0
                  }
                }
              }
            }
          }
        }
      }
    });

console.log("q2_length", x$8);

function q3_last(l) {
  if (!l) {
    return ;
  }
  var t = l.tl;
  var a = l.hd;
  if (t) {
    return Caml_option.some(fold_left((function (param, e) {
                      return e;
                    }), a, t));
  } else {
    return Caml_option.some(a);
  }
}

var x$9 = q3_last({
      hd: 4,
      tl: {
        hd: 6,
        tl: {
          hd: 1,
          tl: {
            hd: 6,
            tl: {
              hd: 4,
              tl: {
                hd: 9,
                tl: {
                  hd: 2,
                  tl: /* [] */0
                }
              }
            }
          }
        }
      }
    });

console.log("q3_last", x$9);

function q4_rev(l) {
  return fold_left((function (a, e) {
                return {
                        hd: e,
                        tl: a
                      };
              }), /* [] */0, l);
}

var l$7 = q4_rev({
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: {
            hd: 4,
            tl: {
              hd: 5,
              tl: /* [] */0
            }
          }
        }
      }
    });

var x$10 = Js_list.toVector(l$7);

console.log("q4_rev", x$10);

function q5_member(x, l) {
  return fold_left((function (a, e) {
                if (e === x) {
                  return true;
                } else {
                  return a;
                }
              }), false, l);
}

var x$11 = q5_member(3, {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: {
            hd: 4,
            tl: {
              hd: 5,
              tl: /* [] */0
            }
          }
        }
      }
    });

console.log("q5_member", x$11);

function q6_sentence(l) {
  return fold_right((function (e, a) {
                return e + " " + a;
              }), l, "");
}

var x$12 = q6_sentence({
      hd: "i",
      tl: {
        hd: "am",
        tl: {
          hd: "good",
          tl: /* [] */0
        }
      }
    });

console.log("q6_sentence", x$12);

function q7_max_depth(t) {
  return fold_tree((function (param, l, r) {
                return 1 + max(l, r) | 0;
              }), 0, t);
}

var x$13 = q7_max_depth(exp_tr);

console.log("q7_max_depth", x$13);

var q8_l = {
  hd: 1,
  tl: {
    hd: 2,
    tl: {
      hd: 3,
      tl: {
        hd: 2,
        tl: {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 2,
              tl: {
                hd: 56,
                tl: {
                  hd: 32,
                  tl: {
                    hd: 2,
                    tl: {
                      hd: 34,
                      tl: {
                        hd: 4,
                        tl: {
                          hd: 2,
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
};

var t = Date.now();

for(var _for = 1; _for <= 10000000; ++_for){
  Belt_List.has(q8_l, q8_l, (function (a, param) {
          return a === 56;
        }));
}

var t$p = Date.now();

for(var _for$1 = 1; _for$1 <= 10000000; ++_for$1){
  q5_member(56, q8_l);
}

var t$p$p = Date.now();

console.log("Our member    took " + (t$p$p - t$p).toString() + " ms");

console.log("Belt.List.has took " + (t$p - t).toString() + " ms");

var List;

export {
  List ,
  log ,
  logList ,
  log2 ,
  logList2 ,
  fold_left ,
  fold_right ,
  max ,
  all ,
  any ,
  map ,
  fold_right_tr ,
  copy ,
  copy_l ,
  append ,
  split ,
  fold_tree ,
  tree_size ,
  tree_sum ,
  exp_tr ,
  tree_preorder ,
  tree_inorder ,
  tree_postorder ,
  q1_deduct ,
  q2_length ,
  q3_last ,
  q4_rev ,
  q5_member ,
  q6_sentence ,
  q7_max_depth ,
  q8_l ,
  t ,
  t$p ,
  t$p$p ,
}
/* prim Not a pure module */
