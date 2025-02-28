// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_int from "rescript/lib/es6/Js_int.js";
import * as Belt_List from "rescript/lib/es6/Belt_List.js";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Primitive_object from "rescript/lib/es6/Primitive_object.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";

function log(prim) {
  console.log(prim);
}

function logList(l) {
  let prim = Stdlib_List.toArray(l);
  console.log(prim);
}

function log2(x, y) {
  console.log(y, x);
}

function logList2(l, str) {
  let x = Stdlib_List.toArray(l);
  console.log(str, x);
}

function fold_left(f, _a, _l) {
  while (true) {
    let l = _l;
    let a = _a;
    if (l === 0) {
      return a;
    }
    _l = l.tl;
    _a = f(a, l.hd);
    continue;
  };
}

let prim = fold_left((prim0, prim1) => prim0 + prim1 | 0, 0, {
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
  if (l !== 0) {
    return f(l.hd, fold_right(f, l.tl, a));
  } else {
    return a;
  }
}

let prim$1 = fold_right((prim0, prim1) => prim0 + prim1 | 0, {
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
  if (Primitive_object.greaterthan(a, b)) {
    return a;
  } else {
    return b;
  }
}

let prim$2 = fold_left((a, b) => Math.max(a, b), Js_int.min, {
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
  return fold_left((prim0, prim1) => {
    if (prim0) {
      return prim1;
    } else {
      return false;
    }
  }, true, l);
}

function any(l) {
  return fold_left((prim0, prim1) => {
    if (prim0) {
      return true;
    } else {
      return prim1;
    }
  }, false, l);
}

function map(f, l) {
  return fold_right((e, a) => ({
    hd: f(e),
    tl: a
  }), l, /* [] */0);
}

let l = map(x => (x << 1), {
  hd: 2,
  tl: {
    hd: 9,
    tl: {
      hd: 1,
      tl: /* [] */0
    }
  }
});

let x = Stdlib_List.toArray(l);

console.log("map", x);

function fold_right_tr(f, l, e) {
  return fold_left((x, y) => f(y, x), e, Stdlib_List.reverse(l));
}

function copy(l) {
  return fold_right((e, a) => ({
    hd: e,
    tl: a
  }), l, /* [] */0);
}

let l$1 = copy({
  hd: 2,
  tl: {
    hd: 5,
    tl: {
      hd: 6,
      tl: /* [] */0
    }
  }
});

let x$1 = Stdlib_List.toArray(l$1);

console.log("copy", x$1);

function copy_l(l) {
  return fold_left((e, a) => ({
    hd: a,
    tl: e
  }), /* [] */0, Stdlib_List.reverse(l));
}

let l$2 = copy_l({
  hd: 2,
  tl: {
    hd: 5,
    tl: {
      hd: 6,
      tl: /* [] */0
    }
  }
});

let x$2 = Stdlib_List.toArray(l$2);

console.log("copy_l", x$2);

function append(x, y) {
  return fold_right((e, a) => ({
    hd: e,
    tl: a
  }), x, y);
}

let l$3 = append({
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

let x$3 = Stdlib_List.toArray(l$3);

console.log("append", x$3);

function split(l) {
  return fold_right((param, param$1) => [
    {
      hd: param[0],
      tl: param$1[0]
    },
    {
      hd: param[1],
      tl: param$1[1]
    }
  ], l, [
    /* [] */0,
    /* [] */0
  ]);
}

let prim$3 = split({
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
  if (typeof t !== "object") {
    return e;
  } else {
    return f(t._0, fold_tree(f, e, t._1), fold_tree(f, e, t._2));
  }
}

function tree_size(t) {
  return fold_tree((param, l, r) => (1 + l | 0) + r | 0, 0, t);
}

function tree_sum(t) {
  return fold_tree((x, l, r) => (x + l | 0) + r | 0, 0, t);
}

let exp_tr = {
  TAG: "Br",
  _0: 1,
  _1: {
    TAG: "Br",
    _0: 0,
    _1: "Lf",
    _2: "Lf"
  },
  _2: {
    TAG: "Br",
    _0: 6,
    _1: {
      TAG: "Br",
      _0: 4,
      _1: "Lf",
      _2: "Lf"
    },
    _2: "Lf"
  }
};

function tree_preorder(t) {
  return fold_tree((x, l, r) => append(append({
    hd: x,
    tl: /* [] */0
  }, l), r), /* [] */0, t);
}

function tree_inorder(t) {
  return fold_tree((x, l, r) => append(append(l, {
    hd: x,
    tl: /* [] */0
  }), r), /* [] */0, t);
}

function tree_postorder(t) {
  return fold_tree((x, l, r) => append(append(l, r), {
    hd: x,
    tl: /* [] */0
  }), /* [] */0, t);
}

let l$4 = tree_preorder(exp_tr);

let x$4 = Stdlib_List.toArray(l$4);

console.log("preorder", x$4);

let l$5 = tree_inorder(exp_tr);

let x$5 = Stdlib_List.toArray(l$5);

console.log("inorder", x$5);

let l$6 = tree_postorder(exp_tr);

let x$6 = Stdlib_List.toArray(l$6);

console.log("postorder", x$6);

function q1_deduct(exp, budget) {
  return fold_left((prim0, prim1) => prim0 - prim1 | 0, budget, exp);
}

let x$7 = q1_deduct({
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
  return fold_right((param, a) => a + 1 | 0, l, 0);
}

let x$8 = q2_length({
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
  if (l === 0) {
    return;
  }
  let t = l.tl;
  let a = l.hd;
  if (t !== 0) {
    return Primitive_option.some(fold_left((param, e) => e, a, t));
  } else {
    return Primitive_option.some(a);
  }
}

let x$9 = q3_last({
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
  return fold_left((a, e) => ({
    hd: e,
    tl: a
  }), /* [] */0, l);
}

let l$7 = q4_rev({
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

let x$10 = Stdlib_List.toArray(l$7);

console.log("q4_rev", x$10);

function q5_member(x, l) {
  return fold_left((a, e) => {
    if (e === x) {
      return true;
    } else {
      return a;
    }
  }, false, l);
}

let x$11 = q5_member(3, {
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
  return fold_right((e, a) => e + " " + a, l, "");
}

let x$12 = q6_sentence({
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
  return fold_tree((param, l, r) => 1 + max(l, r) | 0, 0, t);
}

let x$13 = q7_max_depth(exp_tr);

console.log("q7_max_depth", x$13);

let q8_l = {
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

let t = Date.now();

for (let _for = 1; _for <= 10000000; ++_for) {
  Belt_List.has(q8_l, q8_l, (a, param) => a === 56);
}

let t$p = Date.now();

for (let _for$1 = 1; _for$1 <= 10000000; ++_for$1) {
  q5_member(56, q8_l);
}

let t$p$p = Date.now();

console.log("Our member    took " + (t$p$p - t$p).toString() + " ms");

console.log("Belt.List.has took " + (t$p - t).toString() + " ms");

export {
  log,
  logList,
  log2,
  logList2,
  fold_left,
  fold_right,
  max,
  all,
  any,
  map,
  fold_right_tr,
  copy,
  copy_l,
  append,
  split,
  fold_tree,
  tree_size,
  tree_sum,
  exp_tr,
  tree_preorder,
  tree_inorder,
  tree_postorder,
  q1_deduct,
  q2_length,
  q3_last,
  q4_rev,
  q5_member,
  q6_sentence,
  q7_max_depth,
  q8_l,
  t,
  t$p,
  t$p$p,
}
/* prim Not a pure module */
