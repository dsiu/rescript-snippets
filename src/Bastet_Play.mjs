// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Int$BsBastet from "bs-bastet/src/Int.mjs";
import * as StringLabels from "rescript/lib/es6/stringLabels.js";
import * as Bool$BsBastet from "bs-bastet/src/Bool.mjs";
import * as List$BsBastet from "bs-bastet/src/List.mjs";
import * as Option$BsBastet from "bs-bastet/src/Option.mjs";
import * as Functors$BsBastet from "bs-bastet/src/Functors.mjs";
import * as Functions$BsBastet from "bs-bastet/src/Functions.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var prim = Curry._1(Functors$BsBastet.ListF.$$Option.Traversable.sequence, {
      hd: "foo",
      tl: {
        hd: "bar",
        tl: /* [] */0
      }
    });

console.log(prim);

var prim$1 = Curry._1(Functors$BsBastet.ListF.Int.Show.show, {
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

console.log(prim$1);

var $great$eq$great = Option$BsBastet.Infix.$great$eq$great;

function get_form(param) {
  return {
          name: "Foo",
          address: "123 Bar St."
        };
}

function get_address(form) {
  return form.address;
}

var get_form_address = Curry._2($great$eq$great, get_form, get_address);

var prim$2 = Curry._1(get_form_address, undefined);

console.log(prim$2);

var fmap_add = Functors$BsBastet.ArrayF.Int.Additive.Fold_Map.fold_map;

var __x = [
  1,
  2,
  3,
  4,
  5
];

var prim$3 = Curry._2(fmap_add, Functions$BsBastet.id, __x);

console.log(prim$3);

function trim_all(strings) {
  return Curry._2(List$BsBastet.Infix.$less$$great, StringLabels.trim, strings);
}

var prim$4 = Curry._2(List$BsBastet.Infix.$less$$great, StringLabels.trim, {
      hd: "foo   ",
      tl: {
        hd: "bar",
        tl: {
          hd: "    baz",
          tl: /* [] */0
        }
      }
    });

console.log(prim$4);

function total_score(a, b) {
  return {
          score: Curry._2(Int$BsBastet.Additive.Semigroup.append, a.score, b.score),
          disqualified: Curry._2(Bool$BsBastet.Disjunctive.Semigroup.append, a.disqualified, b.disqualified)
        };
}

var result = total_score({
      score: 4,
      disqualified: false
    }, {
      score: 2,
      disqualified: true
    });

console.log(result);

var T;

export {
  log ,
  log2 ,
  T ,
  $great$eq$great ,
  get_form ,
  get_address ,
  get_form_address ,
  fmap_add ,
  trim_all ,
  total_score ,
  result ,
}
/* prim Not a pure module */
