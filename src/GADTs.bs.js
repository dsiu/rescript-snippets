// Generated by ReScript, PLEASE EDIT WITH CARE


var t1 = 3;

var t2 = "asdf";

var t3 = true;

function $$eval(expr) {
  switch (expr.TAG | 0) {
    case /* Int */0 :
    case /* Bool */1 :
        return expr._0;
    case /* Eq */2 :
        return $$eval(expr._0) === $$eval(expr._1);
    case /* Add */3 :
        return $$eval(expr._0) + $$eval(expr._1) | 0;
    case /* Mul */4 :
        return Math.imul($$eval(expr._0), $$eval(expr._1));
    
  }
}

var isOneEqToOne = {
  TAG: /* Eq */2,
  _0: {
    TAG: /* Int */0,
    _0: 1
  },
  _1: {
    TAG: /* Int */0,
    _0: 1
  }
};

var shouldBeTrue = $$eval(isOneEqToOne);

var is2mult2plus3equalTo7 = {
  TAG: /* Eq */2,
  _0: {
    TAG: /* Add */3,
    _0: {
      TAG: /* Mul */4,
      _0: {
        TAG: /* Int */0,
        _0: 2
      },
      _1: {
        TAG: /* Int */0,
        _0: 2
      }
    },
    _1: {
      TAG: /* Int */0,
      _0: 3
    }
  },
  _1: {
    TAG: /* Int */0,
    _0: 7
  }
};

var shoulBeAlsoTrue = $$eval(is2mult2plus3equalTo7);

console.log(shouldBeTrue);

console.log(shoulBeAlsoTrue);

export {
  t1 ,
  t2 ,
  t3 ,
  $$eval ,
  isOneEqToOne ,
  shouldBeTrue ,
  is2mult2plus3equalTo7 ,
  shoulBeAlsoTrue ,
  
}
/* shouldBeTrue Not a pure module */
