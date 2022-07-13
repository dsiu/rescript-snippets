// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";

function Increment(M) {
  var x = M.x + 1 | 0;
  return {
          x: x
        };
}

var Three = {
  x: 3
};

var x = 4;

var Four = {
  x: x
};

console.log(x - 3 | 0);

var Three_and_more = {
  x: 3,
  y: "three"
};

var x$1 = 4;

var Four_1 = {
  x: x$1
};

var A_Trivial_Example = {
  Increment: Increment,
  Three: Three,
  Four: Four,
  Three_and_more: Three_and_more,
  Four_1: Four_1
};

function Make_interval(Endpoint) {
  var create = function (low, high) {
    if (Curry._2(Endpoint.compare, low, high) > 0) {
      return /* Empty */0;
    } else {
      return /* Interval */{
              _0: low,
              _1: high
            };
    }
  };
  var is_empty = function (x) {
    if (x) {
      return false;
    } else {
      return true;
    }
  };
  var contains = function (t, x) {
    if (t && Curry._2(Endpoint.compare, x, t._0) >= 0) {
      return Curry._2(Endpoint.compare, x, t._1) <= 0;
    } else {
      return false;
    }
  };
  var intersect = function (t1, t2) {
    var min = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) <= 0) {
        return x;
      } else {
        return y;
      }
    };
    var max = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) >= 0) {
        return x;
      } else {
        return y;
      }
    };
    if (t1 && t2) {
      return create(max(t1._0, t2._0), min(t1._1, t2._1));
    } else {
      return /* Empty */0;
    }
  };
  return {
          create: create,
          is_empty: is_empty,
          contains: contains,
          intersect: intersect
        };
}

function create(low, high) {
  if (Caml_obj.caml_compare(low, high) > 0) {
    return /* Empty */0;
  } else {
    return /* Interval */{
            _0: low,
            _1: high
          };
  }
}

function is_empty(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function contains(t, x) {
  if (t && Caml_obj.caml_compare(x, t._0) >= 0) {
    return Caml_obj.caml_compare(x, t._1) <= 0;
  } else {
    return false;
  }
}

function intersect(t1, t2) {
  var min = function (x, y) {
    if (Caml_obj.caml_compare(x, y) <= 0) {
      return x;
    } else {
      return y;
    }
  };
  var max = function (x, y) {
    if (Caml_obj.caml_compare(x, y) >= 0) {
      return x;
    } else {
      return y;
    }
  };
  if (t1 && t2) {
    return create(max(t1._0, t2._0), min(t1._1, t2._1));
  } else {
    return /* Empty */0;
  }
}

var Int_interval = {
  create: create,
  is_empty: is_empty,
  contains: contains,
  intersect: intersect
};

function create$1(low, high) {
  if (Caml_obj.caml_compare(low, high) > 0) {
    return /* Empty */0;
  } else {
    return /* Interval */{
            _0: low,
            _1: high
          };
  }
}

function is_empty$1(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function contains$1(t, x) {
  if (t && Caml_obj.caml_compare(x, t._0) >= 0) {
    return Caml_obj.caml_compare(x, t._1) <= 0;
  } else {
    return false;
  }
}

function intersect$1(t1, t2) {
  var min = function (x, y) {
    if (Caml_obj.caml_compare(x, y) <= 0) {
      return x;
    } else {
      return y;
    }
  };
  var max = function (x, y) {
    if (Caml_obj.caml_compare(x, y) >= 0) {
      return x;
    } else {
      return y;
    }
  };
  if (t1 && t2) {
    return create$1(max(t1._0, t2._0), min(t1._1, t2._1));
  } else {
    return /* Empty */0;
  }
}

var String_interval = {
  create: create$1,
  is_empty: is_empty$1,
  contains: contains$1,
  intersect: intersect$1
};

function compare(param, param$1) {
  return 0;
}

var My_Str = {
  compare: compare
};

function create$2(low, high) {
  return /* Interval */{
          _0: low,
          _1: high
        };
}

function is_empty$2(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function contains$2(t, x) {
  if (t) {
    return true;
  } else {
    return false;
  }
}

function intersect$2(t1, t2) {
  if (t1 && t2) {
    return /* Interval */{
            _0: t1._0,
            _1: t1._1
          };
  } else {
    return /* Empty */0;
  }
}

var My_Str_interval = {
  create: create$2,
  is_empty: is_empty$2,
  contains: contains$2,
  intersect: intersect$2
};

var i1 = create(3, 8);

var i2 = create(4, 10);

console.log(intersect(i1, i2));

function create$3(low, high) {
  if ((low - high | 0) > 0) {
    return /* Empty */0;
  } else {
    return /* Interval */{
            _0: low,
            _1: high
          };
  }
}

function is_empty$3(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function contains$3(t, x) {
  if (t && (x - t._0 | 0) >= 0) {
    return (x - t._1 | 0) <= 0;
  } else {
    return false;
  }
}

function intersect$3(t1, t2) {
  var min = function (x, y) {
    if ((x - y | 0) <= 0) {
      return x;
    } else {
      return y;
    }
  };
  var max = function (x, y) {
    if ((x - y | 0) >= 0) {
      return x;
    } else {
      return y;
    }
  };
  if (t1 && t2) {
    return create$3(max(t1._0, t2._0), min(t1._1, t2._1));
  } else {
    return /* Empty */0;
  }
}

var Rev_int_interval = {
  create: create$3,
  is_empty: is_empty$3,
  contains: contains$3,
  intersect: intersect$3
};

console.log(is_empty(/* Interval */{
          _0: 4,
          _1: 3
        }));

var A_Bigger_Example = {
  Make_interval: Make_interval,
  Int_interval: Int_interval,
  String_interval: String_interval,
  My_Str: My_Str,
  My_Str_interval: My_Str_interval,
  i1: i1,
  i2: i2,
  Rev_int_interval: Rev_int_interval
};

function Make_interval$1(Endpoint) {
  var create = function (low, high) {
    if (Curry._2(Endpoint.compare, low, high) > 0) {
      return /* Empty */0;
    } else {
      return /* Interval */{
              _0: low,
              _1: high
            };
    }
  };
  var is_empty = function (x) {
    if (x) {
      return false;
    } else {
      return true;
    }
  };
  var contains = function (t, x) {
    if (t && Curry._2(Endpoint.compare, x, t._0) >= 0) {
      return Curry._2(Endpoint.compare, x, t._1) <= 0;
    } else {
      return false;
    }
  };
  var intersect = function (t1, t2) {
    var min = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) <= 0) {
        return x;
      } else {
        return y;
      }
    };
    var max = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) >= 0) {
        return x;
      } else {
        return y;
      }
    };
    if (t1 && t2) {
      return create(max(t1._0, t2._0), min(t1._1, t2._1));
    } else {
      return /* Empty */0;
    }
  };
  return {
          create: create,
          is_empty: is_empty,
          contains: contains,
          intersect: intersect
        };
}

function create$4(low, high) {
  if (Caml_obj.caml_compare(low, high) > 0) {
    return /* Empty */0;
  } else {
    return /* Interval */{
            _0: low,
            _1: high
          };
  }
}

function is_empty$4(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function contains$4(t, x) {
  if (t && Caml_obj.caml_compare(x, t._0) >= 0) {
    return Caml_obj.caml_compare(x, t._1) <= 0;
  } else {
    return false;
  }
}

function intersect$4(t1, t2) {
  var min = function (x, y) {
    if (Caml_obj.caml_compare(x, y) <= 0) {
      return x;
    } else {
      return y;
    }
  };
  var max = function (x, y) {
    if (Caml_obj.caml_compare(x, y) >= 0) {
      return x;
    } else {
      return y;
    }
  };
  if (t1 && t2) {
    return create$4(max(t1._0, t2._0), min(t1._1, t2._1));
  } else {
    return /* Empty */0;
  }
}

var Int_interval$1 = {
  create: create$4,
  is_empty: is_empty$4,
  contains: contains$4,
  intersect: intersect$4
};

var i = create$4(3, 4);

console.log(contains$4(i, 5));

var Sharing_Constraints = {
  Make_interval: Make_interval$1,
  Int_interval: Int_interval$1,
  i: i
};

function Make_interval$2(Endpoint) {
  var create = function (low, high) {
    if (Curry._2(Endpoint.compare, low, high) > 0) {
      return /* Empty */0;
    } else {
      return /* Interval */{
              _0: low,
              _1: high
            };
    }
  };
  var is_empty = function (x) {
    if (x) {
      return false;
    } else {
      return true;
    }
  };
  var contains = function (t, x) {
    if (t && Curry._2(Endpoint.compare, x, t._0) >= 0) {
      return Curry._2(Endpoint.compare, x, t._1) <= 0;
    } else {
      return false;
    }
  };
  var intersect = function (t1, t2) {
    var min = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) <= 0) {
        return x;
      } else {
        return y;
      }
    };
    var max = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) >= 0) {
        return x;
      } else {
        return y;
      }
    };
    if (t1 && t2) {
      return create(max(t1._0, t2._0), min(t1._1, t2._1));
    } else {
      return /* Empty */0;
    }
  };
  return {
          create: create,
          is_empty: is_empty,
          contains: contains,
          intersect: intersect
        };
}

function create$5(low, high) {
  if (Caml_obj.caml_compare(low, high) > 0) {
    return /* Empty */0;
  } else {
    return /* Interval */{
            _0: low,
            _1: high
          };
  }
}

function is_empty$5(x) {
  if (x) {
    return false;
  } else {
    return true;
  }
}

function contains$5(t, x) {
  if (t && Caml_obj.caml_compare(x, t._0) >= 0) {
    return Caml_obj.caml_compare(x, t._1) <= 0;
  } else {
    return false;
  }
}

function intersect$5(t1, t2) {
  var min = function (x, y) {
    if (Caml_obj.caml_compare(x, y) <= 0) {
      return x;
    } else {
      return y;
    }
  };
  var max = function (x, y) {
    if (Caml_obj.caml_compare(x, y) >= 0) {
      return x;
    } else {
      return y;
    }
  };
  if (t1 && t2) {
    return create$5(max(t1._0, t2._0), min(t1._1, t2._1));
  } else {
    return /* Empty */0;
  }
}

var Int_interval$2 = {
  create: create$5,
  is_empty: is_empty$5,
  contains: contains$5,
  intersect: intersect$5
};

console.log(is_empty$5(create$5(3, 4)));

var Destructive_Substitution = {
  Make_interval: Make_interval$2,
  Int_interval: Int_interval$2
};

var Sexp = {};

var Sexpable = {};

function Make_interval$3(Endpoint) {
  var create = function (low, high) {
    if (Curry._2(Endpoint.compare, low, high) > 0) {
      return /* Empty */0;
    } else {
      return /* Interval */{
              _0: low,
              _1: high
            };
    }
  };
  var is_empty = function (x) {
    if (x) {
      return false;
    } else {
      return true;
    }
  };
  var contains = function (t, x) {
    if (t && Curry._2(Endpoint.compare, x, t._0) >= 0) {
      return Curry._2(Endpoint.compare, x, t._1) <= 0;
    } else {
      return false;
    }
  };
  var intersect = function (t1, t2) {
    var min = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) <= 0) {
        return x;
      } else {
        return y;
      }
    };
    var max = function (x, y) {
      if (Curry._2(Endpoint.compare, x, y) >= 0) {
        return x;
      } else {
        return y;
      }
    };
    if (t1 && t2) {
      return create(max(t1._0, t2._0), min(t1._1, t2._1));
    } else {
      return /* Empty */0;
    }
  };
  return {
          create: create,
          is_empty: is_empty,
          contains: contains,
          intersect: intersect
        };
}

var Using_Multiple_Interfaces = {
  Sexp: Sexp,
  Sexpable: Sexpable,
  Make_interval: Make_interval$3
};

var Extending_Modules = {};

export {
  A_Trivial_Example ,
  A_Bigger_Example ,
  Sharing_Constraints ,
  Destructive_Substitution ,
  Using_Multiple_Interfaces ,
  Extending_Modules ,
  
}
/*  Not a pure module */
