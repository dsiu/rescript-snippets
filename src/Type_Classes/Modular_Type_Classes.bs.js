// Generated by ReScript, PLEASE EDIT WITH CARE

import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function eq(a, b) {
  return a === b;
}

var Eq_bool = {
  eq: eq
};

function eq$1(a, b) {
  return a === b;
}

var Eq_int = {
  eq: eq$1
};

function Eq_prod(X, Y) {
  var eq = function (param, param$1) {
    if (Curry._2(X.eq, param[0], param$1[0])) {
      return Curry._2(Y.eq, param[1], param$1[1]);
    } else {
      return false;
    }
  };
  return {
          eq: eq
        };
}

function eq$2(param, param$1) {
  if (eq(param[0], param$1[0])) {
    return eq$1(param[1], param$1[1]);
  } else {
    return false;
  }
}

var Eq_bool_int = {
  eq: eq$2
};

var lt = Caml_obj.caml_lessthan;

var Ord_int = {
  eq: eq$1,
  lt: lt
};

function Ord_prod(X, Y) {
  var eq = function (param, param$1) {
    if (Curry._2(X.eq, param[0], param$1[0])) {
      return Curry._2(Y.eq, param[1], param$1[1]);
    } else {
      return false;
    }
  };
  var include = {
    eq: eq
  };
  var lt = function (param, param$1) {
    var x2 = param$1[0];
    var x1 = param[0];
    if (Curry._2(X.lt, x1, x2)) {
      return true;
    } else if (Curry._2(X.eq, x1, x2)) {
      return Curry._2(Y.lt, param[1], param$1[1]);
    } else {
      return false;
    }
  };
  return {
          eq: include.eq,
          lt: lt
        };
}

function eq$3(param, param$1) {
  if (eq$1(param[0], param$1[0])) {
    return eq$1(param[1], param$1[1]);
  } else {
    return false;
  }
}

var include = {
  eq: eq$3
};

function lt$1(param, param$1) {
  var x2 = param$1[0];
  var x1 = param[0];
  if (lt(x1, x2)) {
    return true;
  } else if (eq$1(x1, x2)) {
    return lt(param[1], param$1[1]);
  } else {
    return false;
  }
}

var Ord_int_int_eq = include.eq;

var Ord_int_int = {
  eq: Ord_int_int_eq,
  lt: lt$1
};

var x = [
  1,
  2
];

var y = [
  1,
  4
];

var test_ord_int_int = !Curry._2(Ord_int_int_eq, x, y) && Curry._2(lt$1, x, y);

console.log(test_ord_int_int);

function show(prim) {
  return String(prim);
}

var Show_int = {
  show: show
};

function show$1(x) {
  if (x) {
    return "True";
  } else {
    return "False";
  }
}

var Show_bool = {
  show: show$1
};

function print(show, x) {
  console.log(Curry._1(show.show, x));
  
}

var test_print_1 = print(Show_bool, true);

var test_print_2 = print(Show_int, 3);

function from_int(x) {
  return x;
}

function $plus(x, y) {
  return x + y | 0;
}

var Num_int = {
  from_int: from_int,
  $plus: $plus
};

function from_int$1(x) {
  return x !== 0;
}

function $plus$1(x, y) {
  if (x) {
    return true;
  } else {
    return y;
  }
}

var Num_bool = {
  from_int: from_int$1,
  $plus: $plus$1
};

function sum(num, ls) {
  return List.fold_right(num.$plus, ls, Curry._1(num.from_int, 0));
}

var test_sum = sum(Num_int, {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: {
            hd: 4,
            tl: {
              hd: 20,
              tl: /* [] */0
            }
          }
        }
      }
    });

console.log(test_sum);

function print_incr(param, x) {
  var num = param[1];
  return print(param[0], Curry._2(num.$plus, x, Curry._1(num.from_int, 1)));
}

function print_incr_int(x) {
  return print_incr([
              Show_int,
              Num_int
            ], x);
}

print_incr_int(27);

function List_show(X) {
  var show = function (xs) {
    var go = function (first, x) {
      if (x) {
        return (
                first ? "" : ", "
              ) + (Curry._1(X.show, x.hd) + go(false, x.tl));
      } else {
        return "]";
      }
    };
    return "[" + go(true, xs);
  };
  return {
          show: show
        };
}

function show_list(show) {
  var show$1 = function (xs) {
    var go = function (first, x) {
      if (x) {
        return (
                first ? "" : ", "
              ) + (Curry._1(show.show, x.hd) + go(false, x.tl));
      } else {
        return "]";
      }
    };
    return "[" + go(true, xs);
  };
  return {
          show: show$1
        };
}

var Show = show_list(Show_int);

var testls = Curry._1(Show.show, {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    });

function Mul_default(E, N) {
  var eq = E.eq;
  var from_int = N.from_int;
  var $plus = N.$plus;
  var loop = function (x, y) {
    if (Curry._2(eq, x, Curry._1(from_int, 0))) {
      return Curry._1(from_int, 0);
    } else if (Curry._2(eq, x, Curry._1(from_int, 1))) {
      return y;
    } else {
      return Curry._2($plus, y, loop(Curry._2($plus, x, Curry._1(from_int, -1)), y));
    }
  };
  return {
          eq: eq,
          from_int: from_int,
          $plus: $plus,
          mul: loop
        };
}

function loop(x, y) {
  if (Curry._2(eq, x, Curry._1(from_int$1, 0))) {
    return Curry._1(from_int$1, 0);
  } else if (Curry._2(eq, x, Curry._1(from_int$1, 1))) {
    return y;
  } else {
    return Curry._2($plus$1, y, loop(Curry._2($plus$1, x, Curry._1(from_int$1, -1)), y));
  }
}

var Mul_bool = {
  eq: eq,
  from_int: from_int$1,
  $plus: $plus$1,
  mul: loop
};

function mul(prim0, prim1) {
  return Math.imul(prim0, prim1);
}

var Mul_int = {
  eq: eq$1,
  from_int: from_int,
  $plus: $plus,
  mul: mul
};

function dot(mul, xs, ys) {
  return sum({
              from_int: mul.from_int,
              $plus: mul.$plus
            }, List.map2(mul.mul, xs, ys));
}

var test_dot = dot(Mul_int, {
      hd: 1,
      tl: {
        hd: 2,
        tl: {
          hd: 3,
          tl: /* [] */0
        }
      }
    }, {
      hd: 4,
      tl: {
        hd: 5,
        tl: {
          hd: 6,
          tl: /* [] */0
        }
      }
    });

function replicate(n, x) {
  if (n <= 0) {
    return /* [] */0;
  } else {
    return {
            hd: x,
            tl: replicate(n - 1 | 0, x)
          };
  }
}

function print_nested(show_mod, x) {
  if (x !== 0) {
    return function (x$1) {
      return print_nested(show_list(show_mod), x - 1 | 0)(replicate(x, x$1));
    };
  } else {
    return function (x) {
      return print(show_mod, x);
    };
  }
}

var n = Pervasives.read_int(undefined);

var test_nested = print_nested(Show_int, n)(5);

var show_int = Show_int;

var show_bool = Show_bool;

var num_int = Num_int;

var num_bool = Num_bool;

export {
  Eq_bool ,
  Eq_int ,
  Eq_prod ,
  Eq_bool_int ,
  Ord_int ,
  Ord_prod ,
  Ord_int_int ,
  test_ord_int_int ,
  Show_int ,
  Show_bool ,
  show_int ,
  show_bool ,
  print ,
  test_print_1 ,
  test_print_2 ,
  Num_int ,
  num_int ,
  Num_bool ,
  num_bool ,
  sum ,
  test_sum ,
  print_incr ,
  print_incr_int ,
  List_show ,
  show_list ,
  testls ,
  Mul_default ,
  Mul_bool ,
  Mul_int ,
  dot ,
  test_dot ,
  replicate ,
  print_nested ,
  test_nested ,
  
}
/* test_ord_int_int Not a pure module */
