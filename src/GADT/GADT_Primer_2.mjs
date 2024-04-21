// Generated by ReScript, PLEASE EDIT WITH CARE


function mapOption$p(f, opt) {
  if (typeof opt !== "object") {
    return "None'";
  } else {
    return {
            TAG: "Some'",
            _0: f(opt._0)
          };
  }
}

var a = {
  TAG: "Some'",
  _0: 5
};

function inc(x) {
  return x + 1 | 0;
}

var c = mapOption$p(inc, a);

var d = mapOption$p(inc, "None'");

var myInt = 42;

var myFloat = 4.2;

var myBool = false;

var myStr = "Hello";

var List$p = {};

var myList = {
  TAG: "Con",
  _0: 1,
  _1: {
    TAG: "Con",
    _0: 2,
    _1: {
      TAG: "Con",
      _0: 3,
      _1: "Empty"
    }
  }
};

console.log(myList, "myList");

function length(t) {
  if (typeof t !== "object") {
    return 0;
  } else {
    return 1 + length(t._1) | 0;
  }
}

var HList = {
  length: length
};

var myHeteroList = {
  TAG: "Con",
  _0: 1,
  _1: {
    TAG: "Con",
    _0: 2.5,
    _1: {
      TAG: "Con",
      _0: false,
      _1: {
        TAG: "Con",
        _0: "abc",
        _1: {
          TAG: "Con",
          _0: 5,
          _1: {
            TAG: "Con",
            _0: {
              TAG: "Con",
              _0: 1,
              _1: {
                TAG: "Con",
                _0: "def",
                _1: "Empty"
              }
            },
            _1: "Empty"
          }
        }
      }
    }
  }
};

var myListLength = length(myHeteroList);

console.log(myHeteroList, "myHeteroList");

console.log(myListLength, "myListLength");

function length$1(t) {
  if (typeof t !== "object") {
    return 0;
  } else {
    return 1 + length$1(t._1) | 0;
  }
}

function head(t) {
  return t._0;
}

var SafeList = {
  length: length$1,
  head: head
};

var nonEmptyList = {
  TAG: "Con",
  _0: 1,
  _1: {
    TAG: "Con",
    _0: 2,
    _1: {
      TAG: "Con",
      _0: 3,
      _1: {
        TAG: "Con",
        _0: 4,
        _1: "Empty"
      }
    }
  }
};

var sizeOfNonEmptyList = length$1(nonEmptyList);

var firstElem = head(nonEmptyList);

((function (__x) {
        console.log(__x, "sizeOfNonEmptyList");
      })(sizeOfNonEmptyList));

console.log(firstElem, "firstElem");

var sizeOfEmptyList = length$1("Empty");

console.log("Empty", "emptyList");

console.log(sizeOfEmptyList, "sizeOfEmptyList");

var one = {
  TAG: "Succ",
  _0: "Zero"
};

var two = {
  TAG: "Succ",
  _0: {
    TAG: "Succ",
    _0: "Zero"
  }
};

var three = {
  TAG: "Succ",
  _0: two
};

console.log(one, "one");

console.log(two, "two");

function inc$1(pn) {
  return {
          TAG: "Succ",
          _0: pn
        };
}

var three_ = {
  TAG: "Succ",
  _0: {
    TAG: "Succ",
    _0: {
      TAG: "Succ",
      _0: "Zero"
    }
  }
};

var three__ = {
  TAG: "Succ",
  _0: two
};

function dec(pn) {
  return pn._0;
}

var one_ = {
  TAG: "Succ",
  _0: "Zero"
};

function isEqual(_i, _j) {
  while(true) {
    var j = _j;
    var i = _i;
    if (typeof i !== "object") {
      if (typeof j !== "object") {
        return true;
      } else {
        return false;
      }
    }
    if (typeof j !== "object") {
      return false;
    }
    _j = j._0;
    _i = i._0;
    continue ;
  };
}

var isTwoEqualToOne = isEqual(one, two);

var isThreeEqualToSuccTwo = isEqual({
      TAG: "Succ",
      _0: two
    }, three);

console.log(isTwoEqualToOne, "isTwoEqualToOne");

console.log(isThreeEqualToSuccTwo, "isThreeEqualToSuccTwo");

function $$eval(pn) {
  if (typeof pn !== "object") {
    return 0;
  } else {
    return 1 + $$eval(pn._0) | 0;
  }
}

var threeValue = $$eval(three);

var fourValue = $$eval({
      TAG: "Succ",
      _0: three
    });

console.log(threeValue, "threeValue");

console.log(fourValue, "fourValue");

function length$2(l) {
  if (typeof l !== "object") {
    return 0;
  } else {
    return 1 + length$2(l._1) | 0;
  }
}

function pop(l) {
  return l._1;
}

function push(l, v) {
  if (typeof l !== "object") {
    return {
            TAG: "Con",
            _0: v,
            _1: "Empty"
          };
  } else {
    return {
            TAG: "Con",
            _0: v,
            _1: l
          };
  }
}

var LengthList = {
  length: length$2,
  pop: pop,
  push: push
};

var twoElemList = {
  TAG: "Con",
  _0: 1,
  _1: {
    TAG: "Con",
    _0: 2,
    _1: "Empty"
  }
};

var threeElemList = push(twoElemList, 3);

var oneElemList = pop(pop(threeElemList));

console.log(twoElemList, "twoElemList");

console.log(threeElemList, "threeElemList");

console.log(oneElemList, "oneElemList");

function push_(l, v) {
  return {
          hd: v,
          tl: /* [] */0
        };
}

var b = "None'";

var emptyList = "Empty";

export {
  mapOption$p ,
  a ,
  b ,
  c ,
  d ,
  myInt ,
  myFloat ,
  myBool ,
  myStr ,
  List$p ,
  myList ,
  HList ,
  myHeteroList ,
  myListLength ,
  SafeList ,
  nonEmptyList ,
  sizeOfNonEmptyList ,
  firstElem ,
  emptyList ,
  sizeOfEmptyList ,
  one ,
  two ,
  three ,
  inc$1 as inc,
  three_ ,
  three__ ,
  dec ,
  one_ ,
  isEqual ,
  isTwoEqualToOne ,
  isThreeEqualToSuccTwo ,
  $$eval ,
  threeValue ,
  fourValue ,
  LengthList ,
  twoElemList ,
  threeElemList ,
  oneElemList ,
  push_ ,
}
/* c Not a pure module */
