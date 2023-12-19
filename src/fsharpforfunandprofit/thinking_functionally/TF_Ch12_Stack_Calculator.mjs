// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Stdlib_Function from "@dsiu/rescript-stdlib-fp/src/Stdlib_Function.mjs";

var contents = {
  hd: 1.0,
  tl: {
    hd: 2.0,
    tl: {
      hd: 3.0,
      tl: /* [] */0
    }
  }
};

console.log(contents);

function push(contents, x) {
  return {
          TAG: "StackContents",
          _0: {
            hd: x,
            tl: contents._0
          }
        };
}

var emptyStack = {
  TAG: "StackContents",
  _0: /* [] */0
};

var stackWith1 = push(emptyStack, 1.0);

var stackWith2 = push(stackWith1, 2.0);

console.log(stackWith1);

console.log(stackWith2);

function one(__x) {
  return push(__x, 1.0);
}

function two(__x) {
  return push(__x, 2.0);
}

function three(__x) {
  return push(__x, 3.0);
}

function four(__x) {
  return push(__x, 4.0);
}

function five(__x) {
  return push(__x, 5.0);
}

var empty = {
  TAG: "StackContents",
  _0: /* [] */0
};

var stackWith1$1 = push(empty, 1.0);

var stackWith2$1 = push(stackWith1$1, 2.0);

var stackWith3 = push(stackWith2$1, 3.0);

var result123 = push(push(push(empty, 1.0), 2.0), 3.0);

var result321 = push(push(push(empty, 3.0), 2.0), 1.0);

console.log(result123);

console.log(result321);

function pop(contents) {
  var contents$1 = contents._0;
  if (contents$1) {
    var newStack = {
      TAG: "StackContents",
      _0: contents$1.tl
    };
    return [
            contents$1.hd,
            newStack
          ];
  }
  throw {
        RE_EXN_ID: "Not_found",
        Error: new Error()
      };
}

var initialStack = push(push(empty, 1.0), 2.0);

var match = pop(initialStack);

var poppedStack = match[1];

var match$1 = pop(poppedStack);

function add_(stack) {
  var match = pop(stack);
  var match$1 = pop(match[1]);
  var result = match[0] + match$1[0];
  return push(match$1[1], result);
}

function mul_(stack) {
  var match = pop(stack);
  var match$1 = pop(match[1]);
  var result = match[0] * match$1[0];
  return push(match$1[1], result);
}

function binary(mathFn, stack) {
  var match = pop(stack);
  var match$1 = pop(match[1]);
  var z = Curry._2(mathFn, match$1[0], match[0]);
  return push(match$1[1], z);
}

function float_add(x, y) {
  return x + y;
}

function float_mul(x, y) {
  return x * y;
}

function float_sub(x, y) {
  return x - y;
}

function float_div(x, y) {
  return x / y;
}

function add(__x) {
  return binary(float_add, __x);
}

function mul(__x) {
  return binary(float_mul, __x);
}

function sub(__x) {
  return binary(float_sub, __x);
}

function div(__x) {
  return binary(float_div, __x);
}

console.log(binary(float_div, push(push(empty, 3.0), 2.0)), "threeDivTwo");

console.log(binary(float_sub, push(push(empty, 2.0), 5.0)), "twoSubtractFive");

console.log(binary(float_sub, push(binary(float_add, push(push(empty, 1.0), 2.0)), 3.0)), "oneAddTwoSubThree");

function unary(f, stack) {
  var match = pop(stack);
  return push(match[1], Curry._1(f, match[0]));
}

function neg(param) {
  return unary((function (x) {
                return 0.0 - x;
              }), param);
}

function square(param) {
  return unary((function (x) {
                return x * x;
              }), param);
}

console.log(neg(push(empty, 3.0)), "neg3");

console.log(square(push(empty, 2.0)), "square2");

function show(stack) {
  var match = pop(stack);
  console.log(match[0]);
  return stack;
}

function show2(stack, str) {
  var match = pop(stack);
  console.log(match[0], str);
  return stack;
}

var oneAddTwoSubThree = show2(binary(float_sub, push(binary(float_add, push(push(empty, 1.0), 2.0)), 3.0)), "oneAddTwoSubThree");

function dup(stack) {
  var match = pop(stack);
  return push(stack, match[0]);
}

function swap(stack) {
  var match = pop(stack);
  var match$1 = pop(match[1]);
  return push(push(match$1[1], match[0]), match$1[0]);
}

function drop(stack) {
  return pop(stack)[1];
}

show2(push(push(empty, 1.0), 2.0), "1,2");

show2(binary(float_add, push(show2(binary(float_add, push(push(empty, 1.0), 2.0)), "1+2"), 3.0)), "+3");

show2(binary(float_div, push(show2(binary(float_mul, push(show2(binary(float_add, push(push(empty, 1.0), 2.0)), "1+2"), 3.0)), "*3"), 2.0)), "/2");

function one_two_add(param) {
  return Stdlib_Function.compose((function (param) {
                return Stdlib_Function.compose(one, two, param);
              }), add, param);
}

function one_two_sub(param) {
  return Stdlib_Function.compose((function (param) {
                return Stdlib_Function.compose(one, two, param);
              }), sub, param);
}

show2(one_two_add(empty), "one_two_add");

show2(one_two_sub(empty), "one_two_sub");

function square$1(param) {
  return Stdlib_Function.compose(dup, mul, param);
}

show2(Stdlib_Function.compose(dup, mul, push(empty, 2.0)), "square");

function cube(param) {
  return Stdlib_Function.compose((function (param) {
                return Stdlib_Function.compose((function (param) {
                              return Stdlib_Function.compose(dup, dup, param);
                            }), mul, param);
              }), mul, param);
}

show2(cube(push(empty, 3.0)), "cube");

var sum_numbers_upto = Stdlib_Function.composeN([
      dup,
      one,
      add,
      mul,
      two,
      div
    ]);

show2(Curry._1(sum_numbers_upto, Stdlib_Function.compose(dup, mul, push(empty, 3.0))), "sum up to 9");

function composed_square(param) {
  return Stdlib_Function.compose(dup, mul, param);
}

var stackWith2$2 = push(empty, 2.0);

var twoSquared = binary(float_mul, dup(stackWith2$2));

function lambda_square(param) {
  return unary((function (x) {
                return x * x;
              }), param);
}

var newStack = {
  TAG: "StackContents",
  _0: {
    hd: 1.0,
    tl: {
      hd: 2.0,
      tl: {
        hd: 3.0,
        tl: /* [] */0
      }
    }
  }
};

var popped1 = match[0];

var popped2 = match$1[0];

var poppedStack2 = match$1[1];

var threeDivTwo;

var twoSubtractFive;

var neg3;

var square2;

var start = empty;

var compose = Stdlib_Function.compose;

var composeN = Stdlib_Function.composeN;

export {
  newStack ,
  contents ,
  push ,
  emptyStack ,
  one ,
  two ,
  three ,
  four ,
  five ,
  empty ,
  stackWith1$1 as stackWith1,
  stackWith3 ,
  result123 ,
  result321 ,
  pop ,
  initialStack ,
  popped1 ,
  poppedStack ,
  popped2 ,
  poppedStack2 ,
  add_ ,
  mul_ ,
  binary ,
  float_add ,
  float_mul ,
  float_sub ,
  float_div ,
  add ,
  mul ,
  sub ,
  div ,
  threeDivTwo ,
  twoSubtractFive ,
  unary ,
  neg ,
  neg3 ,
  square2 ,
  show ,
  show2 ,
  oneAddTwoSubThree ,
  dup ,
  swap ,
  drop ,
  start ,
  compose ,
  one_two_add ,
  one_two_sub ,
  square$1 as square,
  cube ,
  composeN ,
  sum_numbers_upto ,
  composed_square ,
  stackWith2$2 as stackWith2,
  twoSquared ,
  lambda_square ,
}
/*  Not a pure module */
