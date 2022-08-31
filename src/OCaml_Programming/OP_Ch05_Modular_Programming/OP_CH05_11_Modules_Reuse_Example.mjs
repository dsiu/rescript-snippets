// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_int32 from "rescript/lib/es6/caml_int32.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function Ring_Make(R) {
  var of_int = function (n) {
    var two = Curry._2(R.$plus, R.one, R.one);
    var loop = function (_n, _b, _x) {
      while(true) {
        var x = _x;
        var b = _b;
        var n = _n;
        if (n === 0) {
          return x;
        }
        _x = n % 2 === 0 ? x : Curry._2(R.$plus, x, b);
        _b = Curry._2(R.$star, b, two);
        _n = n / 2 | 0;
        continue ;
      };
    };
    var m = loop(Pervasives.abs(n), R.one, R.zero);
    if (n < 0) {
      return Curry._1(R.$tilde$neg, m);
    } else {
      return m;
    }
  };
  return {
          zero: R.zero,
          one: R.one,
          $plus: R.$plus,
          $tilde$neg: R.$tilde$neg,
          $star: R.$star,
          to_string: R.to_string,
          of_int: of_int
        };
}

function $plus(prim0, prim1) {
  return prim0 + prim1 | 0;
}

function $tilde$neg(prim) {
  return -prim | 0;
}

function $star(prim0, prim1) {
  return Math.imul(prim0, prim1);
}

function to_string(prim) {
  return String(prim);
}

var Ring_Base_Int = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string
};

function of_int(n) {
  var two = 2;
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b | 0;
      _b = Math.imul(b, two);
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return -m | 0;
  } else {
    return m;
  }
}

var IntRing = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string,
  of_int: of_int
};

function $plus$1(prim0, prim1) {
  return prim0 + prim1;
}

function $tilde$neg$1(prim) {
  return -prim;
}

function $star$1(prim0, prim1) {
  return prim0 * prim1;
}

var Ring_Base_Float = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: Pervasives.string_of_float
};

function of_int$1(n) {
  var two = 1 + 1;
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b;
      _b = b * two;
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return -m;
  } else {
    return m;
  }
}

var FloatRing = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: Pervasives.string_of_float,
  of_int: of_int$1
};

var prim1 = of_int(20);

console.log("IntRing.of_int", prim1);

var prim1$1 = of_int$1(30);

console.log("FloatRing.of_int", prim1$1);

function Field_Make(F) {
  var of_int = function (n) {
    var two = Curry._2(F.$plus, F.one, F.one);
    var loop = function (_n, _b, _x) {
      while(true) {
        var x = _x;
        var b = _b;
        var n = _n;
        if (n === 0) {
          return x;
        }
        _x = n % 2 === 0 ? x : Curry._2(F.$plus, x, b);
        _b = Curry._2(F.$star, b, two);
        _n = n / 2 | 0;
        continue ;
      };
    };
    var m = loop(Pervasives.abs(n), F.one, F.zero);
    if (n < 0) {
      return Curry._1(F.$tilde$neg, m);
    } else {
      return m;
    }
  };
  return {
          zero: F.zero,
          one: F.one,
          $plus: F.$plus,
          $tilde$neg: F.$tilde$neg,
          $star: F.$star,
          to_string: F.to_string,
          $slash: F.$slash,
          of_int: of_int
        };
}

var $slash = Caml_int32.div;

var Field_Base_Int = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string,
  $slash: $slash
};

function of_int$2(n) {
  var two = 2;
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b | 0;
      _b = Math.imul(b, two);
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return -m | 0;
  } else {
    return m;
  }
}

var IntField = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string,
  $slash: $slash,
  of_int: of_int$2
};

function $slash$1(prim0, prim1) {
  return prim0 / prim1;
}

var Field_Base_Float = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: Pervasives.string_of_float,
  $slash: $slash$1
};

function of_int$3(n) {
  var two = 1 + 1;
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b;
      _b = b * two;
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return -m;
  } else {
    return m;
  }
}

var FloatField = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: Pervasives.string_of_float,
  $slash: $slash$1,
  of_int: of_int$3
};

var prim1$2 = Curry._1(of_int$2, 20);

console.log("IntField.of_int", prim1$2);

var prim1$3 = Curry._1(of_int$3, 30);

console.log("FloatField.of_int", prim1$3);

function FractionRing_Make(R) {
  var zero_0 = R.zero;
  var zero_1 = R.one;
  var zero = [
    zero_0,
    zero_1
  ];
  var one_0 = R.one;
  var one_1 = R.one;
  var one = [
    one_0,
    one_1
  ];
  var $plus = function (param, param$1) {
    var d = param$1[1];
    var b = param[1];
    return [
            Curry._2(R.$plus, Curry._2(R.$star, param[0], d), Curry._2(R.$star, param$1[0], b)),
            Curry._2(R.$star, b, d)
          ];
  };
  var $tilde$neg = function (param) {
    return [
            Curry._1(R.$tilde$neg, param[0]),
            param[1]
          ];
  };
  var $star = function (param, param$1) {
    return [
            Curry._2(R.$star, param[0], param$1[0]),
            Curry._2(R.$star, param[1], param$1[1])
          ];
  };
  var to_string = function (param) {
    return Curry._1(R.to_string, param[0]) + "/" + Curry._1(R.to_string, param[1]);
  };
  return {
          zero: zero,
          one: one,
          $plus: $plus,
          $tilde$neg: $tilde$neg,
          $star: $star,
          to_string: to_string
        };
}

var zero = [
  0,
  1
];

var one = [
  1,
  1
];

function $plus$2(param, param$1) {
  var d = param$1[1];
  var b = param[1];
  return [
          Math.imul(param[0], d) + Math.imul(param$1[0], b) | 0,
          Math.imul(b, d)
        ];
}

function $tilde$neg$2(param) {
  return [
          -param[0] | 0,
          param[1]
        ];
}

function $star$2(param, param$1) {
  return [
          Math.imul(param[0], param$1[0]),
          Math.imul(param[1], param$1[1])
        ];
}

function to_string$1(param) {
  return String(param[0]) + "/" + String(param[1]);
}

function of_int$4(n) {
  var two = $plus$2(one, one);
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$2(x, b);
      _b = $star$2(b, two);
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), one, zero);
  if (n < 0) {
    return $tilde$neg$2(m);
  } else {
    return m;
  }
}

var IntRationalRing = {
  zero: zero,
  one: one,
  $plus: $plus$2,
  $tilde$neg: $tilde$neg$2,
  $star: $star$2,
  to_string: to_string$1,
  of_int: of_int$4
};

var zero$1 = [
  0,
  1
];

var one$1 = [
  1,
  1
];

function $plus$3(param, param$1) {
  var d = param$1[1];
  var b = param[1];
  return [
          param[0] * d + param$1[0] * b,
          b * d
        ];
}

function $tilde$neg$3(param) {
  return [
          -param[0],
          param[1]
        ];
}

function $star$3(param, param$1) {
  return [
          param[0] * param$1[0],
          param[1] * param$1[1]
        ];
}

function to_string$2(param) {
  return Pervasives.string_of_float(param[0]) + "/" + Pervasives.string_of_float(param[1]);
}

function of_int$5(n) {
  var two = $plus$3(one$1, one$1);
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$3(x, b);
      _b = $star$3(b, two);
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), one$1, zero$1);
  if (n < 0) {
    return $tilde$neg$3(m);
  } else {
    return m;
  }
}

var FloatRationalRing = {
  zero: zero$1,
  one: one$1,
  $plus: $plus$3,
  $tilde$neg: $tilde$neg$3,
  $star: $star$3,
  to_string: to_string$2,
  of_int: of_int$5
};

function FractionField_Make(F) {
  var zero_0 = F.zero;
  var zero_1 = F.one;
  var zero = [
    zero_0,
    zero_1
  ];
  var one_0 = F.one;
  var one_1 = F.one;
  var one = [
    one_0,
    one_1
  ];
  var $plus = function (param, param$1) {
    var d = param$1[1];
    var b = param[1];
    return [
            Curry._2(F.$plus, Curry._2(F.$star, param[0], d), Curry._2(F.$star, param$1[0], b)),
            Curry._2(F.$star, b, d)
          ];
  };
  var $tilde$neg = function (param) {
    return [
            Curry._1(F.$tilde$neg, param[0]),
            param[1]
          ];
  };
  var $star = function (param, param$1) {
    return [
            Curry._2(F.$star, param[0], param$1[0]),
            Curry._2(F.$star, param[1], param$1[1])
          ];
  };
  var to_string = function (param) {
    return Curry._1(F.to_string, param[0]) + "/" + Curry._1(F.to_string, param[1]);
  };
  var FR = {
    zero: zero,
    one: one,
    $plus: $plus,
    $tilde$neg: $tilde$neg,
    $star: $star,
    to_string: to_string
  };
  var $slash = function (param, param$1) {
    return $star([
                param[0],
                param[1]
              ], [
                param$1[1],
                param$1[0]
              ]);
  };
  return {
          FR: FR,
          zero: zero,
          one: one,
          $plus: $plus,
          $tilde$neg: $tilde$neg,
          $star: $star,
          to_string: to_string,
          $slash: $slash
        };
}

var zero$2 = [
  0,
  1
];

var one$2 = [
  1,
  1
];

function $plus$4(param, param$1) {
  var d = param$1[1];
  var b = param[1];
  return [
          Math.imul(param[0], d) + Math.imul(param$1[0], b) | 0,
          Math.imul(b, d)
        ];
}

function $tilde$neg$4(param) {
  return [
          -param[0] | 0,
          param[1]
        ];
}

function $star$4(param, param$1) {
  return [
          Math.imul(param[0], param$1[0]),
          Math.imul(param[1], param$1[1])
        ];
}

function to_string$3(param) {
  return String(param[0]) + "/" + String(param[1]);
}

function $slash$2(param, param$1) {
  return $star$4([
              param[0],
              param[1]
            ], [
              param$1[1],
              param$1[0]
            ]);
}

function of_int$6(n) {
  var two = $plus$4(one$2, one$2);
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$4(x, b);
      _b = $star$4(b, two);
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), one$2, zero$2);
  if (n < 0) {
    return $tilde$neg$4(m);
  } else {
    return m;
  }
}

var IntRationalField = {
  zero: zero$2,
  one: one$2,
  $plus: $plus$4,
  $tilde$neg: $tilde$neg$4,
  $star: $star$4,
  to_string: to_string$3,
  $slash: $slash$2,
  of_int: of_int$6
};

var zero$3 = [
  0,
  1
];

var one$3 = [
  1,
  1
];

function $plus$5(param, param$1) {
  var d = param$1[1];
  var b = param[1];
  return [
          param[0] * d + param$1[0] * b,
          b * d
        ];
}

function $tilde$neg$5(param) {
  return [
          -param[0],
          param[1]
        ];
}

function $star$5(param, param$1) {
  return [
          param[0] * param$1[0],
          param[1] * param$1[1]
        ];
}

function to_string$4(param) {
  return Pervasives.string_of_float(param[0]) + "/" + Pervasives.string_of_float(param[1]);
}

function $slash$3(param, param$1) {
  return $star$5([
              param[0],
              param[1]
            ], [
              param$1[1],
              param$1[0]
            ]);
}

function of_int$7(n) {
  var two = $plus$5(one$3, one$3);
  var loop = function (_n, _b, _x) {
    while(true) {
      var x = _x;
      var b = _b;
      var n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$5(x, b);
      _b = $star$5(b, two);
      _n = n / 2 | 0;
      continue ;
    };
  };
  var m = loop(Pervasives.abs(n), one$3, zero$3);
  if (n < 0) {
    return $tilde$neg$5(m);
  } else {
    return m;
  }
}

var FloatRationalField = {
  zero: zero$3,
  one: one$3,
  $plus: $plus$5,
  $tilde$neg: $tilde$neg$5,
  $star: $star$5,
  to_string: to_string$4,
  $slash: $slash$3,
  of_int: of_int$7
};

export {
  log ,
  log2 ,
  Ring_Make ,
  Ring_Base_Int ,
  IntRing ,
  Ring_Base_Float ,
  FloatRing ,
  Field_Make ,
  Field_Base_Int ,
  IntField ,
  Field_Base_Float ,
  FloatField ,
  FractionRing_Make ,
  IntRationalRing ,
  FloatRationalRing ,
  FractionField_Make ,
  IntRationalField ,
  FloatRationalField ,
}
/* prim1 Not a pure module */
