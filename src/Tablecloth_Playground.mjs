// Generated by ReScript, PLEASE EDIT WITH CARE

import * as TableclothInt from "tablecloth-rescript/src/TableclothInt.mjs";
import * as TableclothMap from "tablecloth-rescript/src/TableclothMap.mjs";
import * as TableclothSet from "tablecloth-rescript/src/TableclothSet.mjs";
import * as TableclothTuple2 from "tablecloth-rescript/src/TableclothTuple2.mjs";
import * as TableclothComparator from "tablecloth-rescript/src/TableclothComparator.mjs";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

var prim = TableclothTuple2.toArray(TableclothTuple2.make(3, 4));

console.log(prim);

function compare(param) {
  return function (param$1) {
    var param$2 = TableclothInt.compare;
    var param$3 = TableclothInt.compare;
    return TableclothTuple2.compare(param, param$1, param$2, param$3);
  };
}

var include = TableclothComparator.Make({
      compare: compare
    });

var comparator = include.comparator;

var Point = {
  compare: compare,
  comparator: comparator
};

var points = TableclothSet.fromArray([
      [
        0,
        0
      ],
      [
        3,
        4
      ],
      [
        6,
        7
      ]
    ], {
      comparator: comparator
    });

var prim$1 = TableclothSet.toArray(points);

console.log(prim$1);

var pointToAnimal = TableclothMap.fromArray({
      comparator: comparator
    }, [
      [
        [
          0,
          0
        ],
        "Cow"
      ],
      [
        [
          3,
          4
        ],
        "Sheep"
      ],
      [
        [
          6,
          7
        ],
        "Pig"
      ]
    ]);

function a(x) {
  return x;
}

var b = pointToAnimal;

export {
  log ,
  log2 ,
  Point ,
  points ,
  pointToAnimal ,
  a ,
  b ,
}
/* prim Not a pure module */
