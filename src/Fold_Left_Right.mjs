// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function fold_left_(f, e, s) {
  return List.rev(List.fold_right((function (a, acc) {
                    return Curry._2(f, acc, a);
                  }), e, List.rev(s)));
}

function fold_left(f, e, s) {
  return Curry._1(List.fold_right((function (a, acc, x) {
                    return Curry._1(acc, Curry._2(f, x, a));
                  }), s, (function (x) {
                    return x;
                  })), e);
}

var prim = Belt_List.toArray(fold_left((function (acc, x) {
            return {
                    hd: Math.imul(x, x),
                    tl: acc
                  };
          }), /* [] */0, {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }));

console.log(prim);

var prim$1 = Belt_List.toArray(List.fold_right((function (x, acc) {
            return {
                    hd: Math.imul(x, x),
                    tl: acc
                  };
          }), {
          hd: 1,
          tl: {
            hd: 2,
            tl: {
              hd: 3,
              tl: /* [] */0
            }
          }
        }, /* [] */0));

console.log(prim$1);

var fold_right = List.fold_right;

export {
  log ,
  log2 ,
  fold_right ,
  fold_left_ ,
  fold_left ,
}
/* prim Not a pure module */