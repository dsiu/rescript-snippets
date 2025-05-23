// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Belt_Array from "rescript/lib/es6/Belt_Array.js";
import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Stdlib_Int from "rescript/lib/es6/Stdlib_Int.js";
import * as Belt_MapInt from "rescript/lib/es6/Belt_MapInt.js";
import * as Stdlib_List from "rescript/lib/es6/Stdlib_List.js";
import * as Stdlib_Array from "rescript/lib/es6/Stdlib_Array.js";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";
import * as Stdlib_Option from "rescript/lib/es6/Stdlib_Option.js";
import * as Belt_MapString from "rescript/lib/es6/Belt_MapString.js";
import * as Stdlib_Ordering from "rescript/lib/es6/Stdlib_Ordering.js";
import * as Primitive_bigint from "rescript/lib/es6/Primitive_bigint.js";
import * as Primitive_option from "rescript/lib/es6/Primitive_option.js";
import * as Stdlib__Function from "@dsiu/rescript-stdlib-fp/src/Stdlib__Function.res.mjs";
import * as Belt_HashMapString from "rescript/lib/es6/Belt_HashMapString.js";
import * as Belt_MutableMapInt from "rescript/lib/es6/Belt_MutableMapInt.js";
import * as Belt_MutableMapString from "rescript/lib/es6/Belt_MutableMapString.js";

function toString(m, f) {
  return Belt_MapString.reduce(m, "", (a, k, v) => a + ("key:" + k + ", val:" + f(v) + "\n"));
}

function toString$1(m) {
  return toString(m, prim => prim);
}

let $$String = {
  toString: toString$1
};

(1).toString();

function toString$2(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString(m, __x => __x.toString(radix));
}

let Int = {
  toString: toString$2
};

let MapString = {
  toString: toString,
  $$String: $$String,
  Int: Int
};

function toString$3(m, f) {
  return Belt_MapInt.reduce(m, "", (a, k, v) => a + ("key:" + k.toString() + ", val:" + f(v) + "\n"));
}

function toString$4(m) {
  return toString$3(m, prim => prim);
}

let $$String$1 = {
  toString: toString$4
};

function toString$5(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$3(m, __x => __x.toString(radix));
}

let Int$1 = {
  toString: toString$5
};

function toString$6(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$3(m, __x => __x.toString(radix));
}

let $$BigInt = {
  toString: toString$6
};

let MapInt = {
  toString: toString$3,
  $$String: $$String$1,
  Int: Int$1,
  $$BigInt: $$BigInt
};

function toString$7(m, f) {
  return Belt_MutableMapInt.reduce(m, "", (a, k, v) => a + ("key:" + k.toString() + ", val:" + f(v) + "\n"));
}

function toString$8(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$7(m, __x => __x.toString(radix));
}

let Int$2 = {
  toString: toString$8
};

function toString$9(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$7(m, __x => __x.toString(radix));
}

let $$BigInt$1 = {
  toString: toString$9
};

let MutableMapInt = {
  toString: toString$7,
  Int: Int$2,
  $$BigInt: $$BigInt$1
};

function toString$10(m, f) {
  return Belt_MutableMapString.reduce(m, "", (a, k, v) => a + ("key:" + k + ", val:" + f(v) + "\n"));
}

function toString$11(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$10(m, __x => __x.toString(radix));
}

let Int$3 = {
  toString: toString$11
};

function toString$12(m, radixOpt) {
  let radix = radixOpt !== undefined ? radixOpt : 10;
  return toString$10(m, __x => __x.toString(radix));
}

let $$BigInt$2 = {
  toString: toString$12
};

let MutableMapString = {
  toString: toString$10,
  Int: Int$3,
  $$BigInt: $$BigInt$2
};

function toString$13(a, f) {
  return "[" + a.map(f).join(",") + "]";
}

let $$Array = {
  toString: toString$13
};

function toString$14(a, f) {
  return Stdlib_List.reduce(a, "{", (a, v) => a + f(v) + ",") + "}";
}

let List = {
  toString: toString$14
};

let Printable = {
  MapString: MapString,
  MapInt: MapInt,
  MutableMapInt: MutableMapInt,
  MutableMapString: MutableMapString,
  $$Array: $$Array,
  List: List
};

function base2(__x) {
  return __x.toString(2);
}

function compose(f, g) {
  return extra => Stdlib__Function.compose(f, g, extra);
}

function g(prim) {
  return prim;
}

function f(none) {
  return Stdlib_Int.fromString(none, 10);
}

function g$1(extra) {
  return Stdlib__Function.compose(f, g, extra);
}

function f$1(prim) {
  return prim.trim();
}

function intFromStringExn(extra) {
  return Stdlib__Function.compose(f$1, g$1, extra);
}

function add(x, y) {
  return x + y | 0;
}

function sub(x, y) {
  return x - y | 0;
}

function mul(x, y) {
  return x * y | 0;
}

let div = Primitive_int.div;

function int32ToUint32(x) {
  return new Uint32Array([x])[0];
}

function increaseByBigInt(v, n) {
  return Stdlib_Option.mapOr(v, n, x => x + n);
}

function increaseBy1L(__x) {
  return increaseByBigInt(__x, BigInt(1));
}

function increaseBy(v, n) {
  return Stdlib_Option.mapOr(v, n, x => x + n | 0);
}

function increaseBy1(__x) {
  return increaseBy(__x, 1);
}

function splitChars(__x) {
  return __x.split("");
}

function splitSpace(__x) {
  return __x.split(" ");
}

function splitNewline(__x) {
  return __x.split("\n");
}

function splitDoubleNewline(__x) {
  return __x.split("\n\n");
}

function sumIntArray(__x) {
  return Stdlib_Array.reduce(__x, 0, add);
}

function mulIntArray(__x) {
  return Stdlib_Array.reduce(__x, 1, mul);
}

function join(__x) {
  return __x.join("");
}

function sumRange(xs, offset, len) {
  let elems = Belt_Array.slice(xs, offset, len);
  let total = {
    contents: 0
  };
  elems.forEach(x => {
    total.contents = total.contents + x | 0;
  });
  return total.contents;
}

function maxIntInArray(xs) {
  let sorted = xs.toSorted(Primitive_int.compare);
  return sorted[sorted.length - 1 | 0];
}

function minIntInArray(xs) {
  let sorted = xs.toSorted(Primitive_int.compare);
  return sorted[0];
}

function compare(a, b) {
  if (a < b) {
    return -1;
  } else if (a > b) {
    return 1;
  } else {
    return 0;
  }
}

let BigIntExt = {
  compare: compare
};

function maxBigIntInArray(xs) {
  let sorted = xs.toSorted(compare);
  return sorted[sorted.length - 1 | 0];
}

function minBigIntInArray(xs) {
  let sorted = xs.toSorted(compare);
  return sorted[0];
}

function flatten(xs) {
  return Stdlib_Array.reduce(xs, [], (a, x) => Belt_Array.concatMany([
    a,
    x
  ]));
}

function maxKeyIntValuePair(__x) {
  return Stdlib_Array.reduce(__x, [
    "",
    Pervasives.min_int
  ], (acc, param) => {
    let v = param[1];
    if (v > acc[1]) {
      return [
        param[0],
        v
      ];
    } else {
      return acc;
    }
  });
}

function minKeyIntValuePair(__x) {
  return Stdlib_Array.reduce(__x, [
    "",
    Pervasives.max_int
  ], (acc, param) => {
    let v = param[1];
    if (v < acc[1]) {
      return [
        param[0],
        v
      ];
    } else {
      return acc;
    }
  });
}

function keyCompareBigIntValuePair(xs, cmp) {
  let first = xs[0];
  let rest = xs.slice(1);
  return Stdlib_Option.map(first, param => Stdlib_Array.reduce(rest, [
    "",
    param[1]
  ], (acc, param) => {
    let v = param[1];
    if (cmp(Primitive_bigint.compare(v, acc[1]))) {
      return [
        param[0],
        v
      ];
    } else {
      return acc;
    }
  }));
}

function maxKeyBigIntValuePair(__x) {
  return keyCompareBigIntValuePair(__x, Stdlib_Ordering.isGreater);
}

function minKeyBigIntValuePair(__x) {
  return keyCompareBigIntValuePair(__x, Stdlib_Ordering.isLess);
}

function hashMapStringUpdate(h, k, f) {
  Belt_HashMapString.set(h, k, Stdlib_Option.mapOr(Belt_HashMapString.get(h, k), f(undefined), x => f(Primitive_option.some(x))));
  return h;
}

function mutableMapStringUpdate(h, k, f) {
  Belt_MutableMapString.set(h, k, Stdlib_Option.mapOr(Belt_MutableMapString.get(h, k), f(undefined), x => f(Primitive_option.some(x))));
  return h;
}

export {
  Printable,
  base2,
  compose,
  intFromStringExn,
  add,
  sub,
  mul,
  div,
  int32ToUint32,
  increaseByBigInt,
  increaseBy1L,
  increaseBy,
  increaseBy1,
  splitChars,
  splitSpace,
  splitNewline,
  splitDoubleNewline,
  sumIntArray,
  mulIntArray,
  join,
  sumRange,
  maxIntInArray,
  minIntInArray,
  BigIntExt,
  maxBigIntInArray,
  minBigIntInArray,
  flatten,
  maxKeyIntValuePair,
  minKeyIntValuePair,
  keyCompareBigIntValuePair,
  maxKeyBigIntValuePair,
  minKeyBigIntValuePair,
  hashMapStringUpdate,
  mutableMapStringUpdate,
}
/*  Not a pure module */
