// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Pervasives from "rescript/lib/es6/Pervasives.js";
import * as Primitive_int from "rescript/lib/es6/Primitive_int.js";

function log(prim) {
  console.log(prim);
}

function log2(prim0, prim1) {
  console.log(prim0, prim1);
}

function Ring_Make(R) {
  let of_int = n => {
    let two = R.$plus(R.one, R.one);
    let loop = (_n, _b, _x) => {
      while (true) {
        let x = _x;
        let b = _b;
        let n = _n;
        if (n === 0) {
          return x;
        }
        _x = n % 2 === 0 ? x : R.$plus(x, b);
        _b = R.$star(b, two);
        _n = n / 2 | 0;
        continue;
      };
    };
    let m = loop(Pervasives.abs(n), R.one, R.zero);
    if (n < 0) {
      return R.$tilde$neg(m);
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

let Ring_Base_Int = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string
};

function of_int(n) {
  let two = 2;
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b | 0;
      _b = Math.imul(b, two);
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return -m | 0;
  } else {
    return m;
  }
}

let IntRing = {
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
  return - prim;
}

function $star$1(prim0, prim1) {
  return prim0 * prim1;
}

function to_string$1(__x) {
  return __x.toString();
}

let Ring_Base_Float = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: to_string$1
};

function of_int$1(n) {
  let two = 1 + 1;
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b;
      _b = b * two;
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return - m;
  } else {
    return m;
  }
}

let FloatRing = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: to_string$1,
  of_int: of_int$1
};

let __x = of_int(20);

console.log("IntRing.of_int", __x);

let __x$1 = of_int$1(30);

console.log("FloatRing.of_int", __x$1);

function Field_Make(F) {
  let of_int = n => {
    let two = F.$plus(F.one, F.one);
    let loop = (_n, _b, _x) => {
      while (true) {
        let x = _x;
        let b = _b;
        let n = _n;
        if (n === 0) {
          return x;
        }
        _x = n % 2 === 0 ? x : F.$plus(x, b);
        _b = F.$star(b, two);
        _n = n / 2 | 0;
        continue;
      };
    };
    let m = loop(Pervasives.abs(n), F.one, F.zero);
    if (n < 0) {
      return F.$tilde$neg(m);
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

let $slash = Primitive_int.div;

let Field_Base_Int = {
  zero: 0,
  one: 1,
  $plus: $plus,
  $tilde$neg: $tilde$neg,
  $star: $star,
  to_string: to_string,
  $slash: $slash
};

function of_int$2(n) {
  let two = 2;
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b | 0;
      _b = Math.imul(b, two);
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return -m | 0;
  } else {
    return m;
  }
}

let IntField = {
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

let Field_Base_Float = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: to_string$1,
  $slash: $slash$1
};

function of_int$3(n) {
  let two = 1 + 1;
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : x + b;
      _b = b * two;
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), 1, 0);
  if (n < 0) {
    return - m;
  } else {
    return m;
  }
}

let FloatField = {
  zero: 0,
  one: 1,
  $plus: $plus$1,
  $tilde$neg: $tilde$neg$1,
  $star: $star$1,
  to_string: to_string$1,
  $slash: $slash$1,
  of_int: of_int$3
};

let __x$2 = of_int$2(20);

console.log("IntField.of_int", __x$2);

let __x$3 = of_int$3(30);

console.log("FloatField.of_int", __x$3);

function FractionRing_Make(R) {
  let zero_0 = R.zero;
  let zero_1 = R.one;
  let zero = [
    zero_0,
    zero_1
  ];
  let one_0 = R.one;
  let one_1 = R.one;
  let one = [
    one_0,
    one_1
  ];
  let $plus = (param, param$1) => {
    let d = param$1[1];
    let b = param[1];
    return [
      R.$plus(R.$star(param[0], d), R.$star(param$1[0], b)),
      R.$star(b, d)
    ];
  };
  let $tilde$neg = param => [
    R.$tilde$neg(param[0]),
    param[1]
  ];
  let $star = (param, param$1) => [
    R.$star(param[0], param$1[0]),
    R.$star(param[1], param$1[1])
  ];
  let to_string = param => R.to_string(param[0]) + "/" + R.to_string(param[1]);
  return {
    zero: zero,
    one: one,
    $plus: $plus,
    $tilde$neg: $tilde$neg,
    $star: $star,
    to_string: to_string
  };
}

let zero = [
  0,
  1
];

let one = [
  1,
  1
];

function $plus$2(param, param$1) {
  let d = param$1[1];
  let b = param[1];
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

function to_string$2(param) {
  return String(param[0]) + "/" + String(param[1]);
}

function of_int$4(n) {
  let two = $plus$2(one, one);
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$2(x, b);
      _b = $star$2(b, two);
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), one, zero);
  if (n < 0) {
    return $tilde$neg$2(m);
  } else {
    return m;
  }
}

let IntRationalRing = {
  zero: zero,
  one: one,
  $plus: $plus$2,
  $tilde$neg: $tilde$neg$2,
  $star: $star$2,
  to_string: to_string$2,
  of_int: of_int$4
};

let zero$1 = [
  0,
  1
];

let one$1 = [
  1,
  1
];

function $plus$3(param, param$1) {
  let d = param$1[1];
  let b = param[1];
  return [
    param[0] * d + param$1[0] * b,
    b * d
  ];
}

function $tilde$neg$3(param) {
  return [
    - param[0],
    param[1]
  ];
}

function $star$3(param, param$1) {
  return [
    param[0] * param$1[0],
    param[1] * param$1[1]
  ];
}

function to_string$3(param) {
  return param[0].toString() + "/" + param[1].toString();
}

function of_int$5(n) {
  let two = $plus$3(one$1, one$1);
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$3(x, b);
      _b = $star$3(b, two);
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), one$1, zero$1);
  if (n < 0) {
    return $tilde$neg$3(m);
  } else {
    return m;
  }
}

let FloatRationalRing = {
  zero: zero$1,
  one: one$1,
  $plus: $plus$3,
  $tilde$neg: $tilde$neg$3,
  $star: $star$3,
  to_string: to_string$3,
  of_int: of_int$5
};

function FractionField_Make(F) {
  let zero_0 = F.zero;
  let zero_1 = F.one;
  let zero = [
    zero_0,
    zero_1
  ];
  let one_0 = F.one;
  let one_1 = F.one;
  let one = [
    one_0,
    one_1
  ];
  let $plus = (param, param$1) => {
    let d = param$1[1];
    let b = param[1];
    return [
      F.$plus(F.$star(param[0], d), F.$star(param$1[0], b)),
      F.$star(b, d)
    ];
  };
  let $tilde$neg = param => [
    F.$tilde$neg(param[0]),
    param[1]
  ];
  let $star = (param, param$1) => [
    F.$star(param[0], param$1[0]),
    F.$star(param[1], param$1[1])
  ];
  let to_string = param => F.to_string(param[0]) + "/" + F.to_string(param[1]);
  let FR = {
    zero: zero,
    one: one,
    $plus: $plus,
    $tilde$neg: $tilde$neg,
    $star: $star,
    to_string: to_string
  };
  let $slash = (param, param$1) => $star([
    param[0],
    param[1]
  ], [
    param$1[1],
    param$1[0]
  ]);
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

let zero$2 = [
  0,
  1
];

let one$2 = [
  1,
  1
];

function $plus$4(param, param$1) {
  let d = param$1[1];
  let b = param[1];
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

function to_string$4(param) {
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
  let two = $plus$4(one$2, one$2);
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$4(x, b);
      _b = $star$4(b, two);
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), one$2, zero$2);
  if (n < 0) {
    return $tilde$neg$4(m);
  } else {
    return m;
  }
}

let IntRationalField = {
  zero: zero$2,
  one: one$2,
  $plus: $plus$4,
  $tilde$neg: $tilde$neg$4,
  $star: $star$4,
  to_string: to_string$4,
  $slash: $slash$2,
  of_int: of_int$6
};

let zero$3 = [
  0,
  1
];

let one$3 = [
  1,
  1
];

function $plus$5(param, param$1) {
  let d = param$1[1];
  let b = param[1];
  return [
    param[0] * d + param$1[0] * b,
    b * d
  ];
}

function $tilde$neg$5(param) {
  return [
    - param[0],
    param[1]
  ];
}

function $star$5(param, param$1) {
  return [
    param[0] * param$1[0],
    param[1] * param$1[1]
  ];
}

function to_string$5(param) {
  return param[0].toString() + "/" + param[1].toString();
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
  let two = $plus$5(one$3, one$3);
  let loop = (_n, _b, _x) => {
    while (true) {
      let x = _x;
      let b = _b;
      let n = _n;
      if (n === 0) {
        return x;
      }
      _x = n % 2 === 0 ? x : $plus$5(x, b);
      _b = $star$5(b, two);
      _n = n / 2 | 0;
      continue;
    };
  };
  let m = loop(Pervasives.abs(n), one$3, zero$3);
  if (n < 0) {
    return $tilde$neg$5(m);
  } else {
    return m;
  }
}

let FloatRationalField = {
  zero: zero$3,
  one: one$3,
  $plus: $plus$5,
  $tilde$neg: $tilde$neg$5,
  $star: $star$5,
  to_string: to_string$5,
  $slash: $slash$3,
  of_int: of_int$7
};

export {
  log,
  log2,
  Ring_Make,
  Ring_Base_Int,
  IntRing,
  Ring_Base_Float,
  FloatRing,
  Field_Make,
  Field_Base_Int,
  IntField,
  Field_Base_Float,
  FloatField,
  FractionRing_Make,
  IntRationalRing,
  FloatRationalRing,
  FractionField_Make,
  IntRationalField,
  FloatRationalField,
}
/* __x Not a pure module */
