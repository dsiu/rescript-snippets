// Generated by ReScript, PLEASE EDIT WITH CARE

import * as PervasivesU from "rescript/lib/es6/pervasivesU.js";

function go_down(param) {
  var t = param._0;
  if (t.TAG === "Item") {
    return PervasivesU.failwith("down of item");
  }
  var match = t._0;
  if (match) {
    return {
            TAG: "Loc",
            _0: match.hd,
            _1: {
              TAG: "Node",
              _0: /* [] */0,
              _1: param._1,
              _2: match.tl
            }
          };
  } else {
    return PervasivesU.failwith("down of empty");
  }
}

function nth(loc, n) {
  var nthrec = function (n) {
    if (n !== 1) {
      if (n > 0) {
        var param = nthrec(n - 1 | 0);
        var p = param._1;
        if (typeof p !== "object") {
          return PervasivesU.failwith("right of top");
        }
        var match = p._2;
        if (match) {
          return {
                  TAG: "Loc",
                  _0: match.hd,
                  _1: {
                    TAG: "Node",
                    _0: {
                      hd: param._0,
                      tl: p._0
                    },
                    _1: p._1,
                    _2: match.tl
                  }
                };
        } else {
          return PervasivesU.failwith("right of last");
        }
      } else {
        return PervasivesU.failwith("nth expects a positive integer");
      }
    } else {
      return go_down(loc);
    }
  };
  return nthrec(n);
}

function insert_right(param, r) {
  var p = param._1;
  if (typeof p !== "object") {
    return PervasivesU.failwith("insert of top");
  } else {
    return {
            TAG: "Loc",
            _0: param._0,
            _1: {
              TAG: "Node",
              _0: p._0,
              _1: p._1,
              _2: {
                hd: r,
                tl: p._2
              }
            }
          };
  }
}

function insert_left(param, l) {
  var p = param._1;
  if (typeof p !== "object") {
    return PervasivesU.failwith("insert of top");
  } else {
    return {
            TAG: "Loc",
            _0: param._0,
            _1: {
              TAG: "Node",
              _0: {
                hd: l,
                tl: p._0
              },
              _1: p._1,
              _2: p._2
            }
          };
  }
}

function insert_down(param, t1) {
  var t = param._0;
  if (t.TAG === "Item") {
    return PervasivesU.failwith("down of item");
  } else {
    return {
            TAG: "Loc",
            _0: t1,
            _1: {
              TAG: "Node",
              _0: /* [] */0,
              _1: param._1,
              _2: t._0
            }
          };
  }
}

function $$delete(param) {
  var p = param._1;
  if (typeof p !== "object") {
    return PervasivesU.failwith("delete of top");
  }
  var left = p._0;
  var match = p._2;
  if (match) {
    return {
            TAG: "Loc",
            _0: match.hd,
            _1: {
              TAG: "Node",
              _0: left,
              _1: p._1,
              _2: match.tl
            }
          };
  } else if (left) {
    return {
            TAG: "Loc",
            _0: left.hd,
            _1: {
              TAG: "Node",
              _0: left.tl,
              _1: p._1,
              _2: /* [] */0
            }
          };
  } else {
    return {
            TAG: "Loc",
            _0: {
              TAG: "Section",
              _0: /* [] */0
            },
            _1: p._1
          };
  }
}

function go_up_memo(param) {
  var p = param._1;
  if (typeof p !== "object") {
    return PervasivesU.failwith("up of top");
  } else {
    return {
            TAG: "Loc",
            _0: {
              TAG: "Siblings",
              _0: p._0,
              _1: param._0,
              _2: p._2
            },
            _1: p._1
          };
  }
}

function go_down_memo(param) {
  var t = param._0;
  if (t.TAG === "Item") {
    return PervasivesU.failwith("down of item");
  } else {
    return {
            TAG: "Loc",
            _0: t._1,
            _1: {
              TAG: "Node",
              _0: t._0,
              _1: param._1,
              _2: t._2
            }
          };
  }
}

function change(param, t) {
  return {
          TAG: "Loc",
          _0: t,
          _1: param._1
        };
}

function go_left(param) {
  var p = param._1;
  if (typeof p !== "object") {
    return PervasivesU.failwith("left of top");
  } else if (p.TAG === "Left") {
    return PervasivesU.failwith("left of Left");
  } else {
    return {
            TAG: "Loc",
            _0: p._0,
            _1: {
              TAG: "Left",
              _0: p._1,
              _1: param._0
            }
          };
  }
}

function go_right(param) {
  var p = param._1;
  if (typeof p !== "object") {
    return PervasivesU.failwith("right of top");
  } else if (p.TAG === "Left") {
    return {
            TAG: "Loc",
            _0: p._1,
            _1: {
              TAG: "Right",
              _0: param._0,
              _1: p._0
            }
          };
  } else {
    return PervasivesU.failwith("right of Right");
  }
}

function go_up(param) {
  var p = param._1;
  var t = param._0;
  if (typeof p !== "object") {
    return PervasivesU.failwith("up of top");
  } else if (p.TAG === "Left") {
    return {
            TAG: "Loc",
            _0: {
              TAG: "Cons",
              _0: t,
              _1: p._1
            },
            _1: p._0
          };
  } else {
    return {
            TAG: "Loc",
            _0: {
              TAG: "Cons",
              _0: p._0,
              _1: t
            },
            _1: p._1
          };
  }
}

function go_first(param) {
  var t = param._0;
  if (typeof t !== "object") {
    return PervasivesU.failwith("first of Nil");
  } else {
    return {
            TAG: "Loc",
            _0: t._0,
            _1: {
              TAG: "Left",
              _0: param._1,
              _1: t._1
            }
          };
  }
}

function go_second(param) {
  var t = param._0;
  if (typeof t !== "object") {
    return PervasivesU.failwith("second of Nil");
  } else {
    return {
            TAG: "Loc",
            _0: t._1,
            _1: {
              TAG: "Right",
              _0: t._0,
              _1: param._1
            }
          };
  }
}

var ex1 = {
  TAG: "Section",
  _0: {
    hd: {
      TAG: "Section",
      _0: {
        hd: {
          TAG: "Item",
          _0: "a"
        },
        tl: {
          hd: {
            TAG: "Item",
            _0: "*"
          },
          tl: {
            hd: {
              TAG: "Item",
              _0: "b"
            },
            tl: /* [] */0
          }
        }
      }
    },
    tl: {
      hd: {
        TAG: "Item",
        _0: "+"
      },
      tl: {
        hd: {
          TAG: "Section",
          _0: {
            hd: {
              TAG: "Item",
              _0: "c"
            },
            tl: {
              hd: {
                TAG: "Item",
                _0: "*"
              },
              tl: {
                hd: {
                  TAG: "Item",
                  _0: "d"
                },
                tl: /* [] */0
              }
            }
          }
        },
        tl: /* [] */0
      }
    }
  }
};

var loc1 = {
  TAG: "Loc",
  _0: {
    TAG: "Item",
    _0: "*"
  },
  _1: {
    TAG: "Node",
    _0: {
      hd: {
        TAG: "Item",
        _0: "c"
      },
      tl: /* [] */0
    },
    _1: {
      TAG: "Node",
      _0: {
        hd: {
          TAG: "Item",
          _0: "+"
        },
        tl: {
          hd: {
            TAG: "Section",
            _0: {
              hd: {
                TAG: "Item",
                _0: "a"
              },
              tl: {
                hd: {
                  TAG: "Item",
                  _0: "*"
                },
                tl: {
                  hd: {
                    TAG: "Item",
                    _0: "b"
                  },
                  tl: /* [] */0
                }
              }
            }
          },
          tl: /* [] */0
        }
      },
      _1: "Top",
      _2: /* [] */0
    },
    _2: {
      hd: {
        TAG: "Item",
        _0: "d"
      },
      tl: /* [] */0
    }
  }
};

export {
  ex1 ,
  loc1 ,
  go_down ,
  nth ,
  insert_right ,
  insert_left ,
  insert_down ,
  $$delete ,
  go_up_memo ,
  go_down_memo ,
  change ,
  go_left ,
  go_right ,
  go_up ,
  go_first ,
  go_second ,
}
/* No side effect */
