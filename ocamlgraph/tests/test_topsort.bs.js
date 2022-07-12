// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Sys from "../../node_modules/rescript/lib/es6/sys.js";
import * as List from "../../node_modules/rescript/lib/es6/list.js";
import * as Pack from "../src/pack.bs.js";
import * as $$Array from "../../node_modules/rescript/lib/es6/array.js";
import * as Curry from "../../node_modules/rescript/lib/es6/curry.js";
import * as Format from "../../node_modules/rescript/lib/es6/format.js";
import * as Caml_array from "../../node_modules/rescript/lib/es6/caml_array.js";
import * as Caml_format from "../../node_modules/rescript/lib/es6/caml_format.js";

function test(checkOpt, iter, n, edges) {
  var check = checkOpt !== undefined ? checkOpt : true;
  var v = $$Array.init(n, Pack.Digraph.V.create);
  var g = Curry._2(Pack.Digraph.create, undefined, undefined);
  $$Array.iter(Curry._1(Pack.Digraph.add_vertex, g), v);
  var build = function (param) {
    return Curry._3(Pack.Digraph.add_edge, g, Caml_array.get(v, param[0]), Caml_array.get(v, param[1]));
  };
  List.iter(build, edges);
  var num = Caml_array.make(n, 0);
  var i = {
    contents: 0
  };
  Curry._2(iter, (function (v) {
          i.contents = i.contents + 1 | 0;
          return Caml_array.set(num, Curry._1(Pack.Digraph.V.label, v), i.contents);
        }), g);
  var r = $$Array.init(n, (function (i) {
          return i;
        }));
  $$Array.sort((function (i, j) {
          return Caml_array.get(num, i) - Caml_array.get(num, j) | 0;
        }), r);
  if (check) {
    for(var v$1 = 0; v$1 < n; ++v$1){
      Curry._1(Format.printf(/* Format */{
                _0: {
                  TAG: /* Int */4,
                  _0: /* Int_d */0,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* Char_literal */12,
                    _0: /* ' ' */32,
                    _1: /* End_of_format */0
                  }
                },
                _1: "%d "
              }), Caml_array.get(r, v$1));
    }
  }
  Format.printf(/* Format */{
        _0: {
          TAG: /* Formatting_lit */17,
          _0: /* Flush_newline */4,
          _1: /* End_of_format */0
        },
        _1: "@."
      });
  var path = Curry._1(Pack.Digraph.PathCheck.check_path, Curry._1(Pack.Digraph.PathCheck.create, g));
  var check_edge = function (param) {
    var y = param[1];
    var x = param[0];
    var vx = Caml_array.get(v, x);
    var vy = Caml_array.get(v, y);
    Curry._4(Format.printf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "x=",
                _1: {
                  TAG: /* Int */4,
                  _0: /* Int_d */0,
                  _1: /* No_padding */0,
                  _2: /* No_precision */0,
                  _3: {
                    TAG: /* String_literal */11,
                    _0: " y=",
                    _1: {
                      TAG: /* Int */4,
                      _0: /* Int_d */0,
                      _1: /* No_padding */0,
                      _2: /* No_precision */0,
                      _3: {
                        TAG: /* String_literal */11,
                        _0: " num(x)=",
                        _1: {
                          TAG: /* Int */4,
                          _0: /* Int_d */0,
                          _1: /* No_padding */0,
                          _2: /* No_precision */0,
                          _3: {
                            TAG: /* String_literal */11,
                            _0: " num(y)=",
                            _1: {
                              TAG: /* Int */4,
                              _0: /* Int_d */0,
                              _1: /* No_padding */0,
                              _2: /* No_precision */0,
                              _3: {
                                TAG: /* Formatting_lit */17,
                                _0: /* Flush_newline */4,
                                _1: /* End_of_format */0
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              },
              _1: "x=%d y=%d num(x)=%d num(y)=%d@."
            }), x, y, Caml_array.get(num, x), Caml_array.get(num, y));
    Curry._2(Format.printf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "x-->y=",
                _1: {
                  TAG: /* Bool */9,
                  _0: /* No_padding */0,
                  _1: {
                    TAG: /* String_literal */11,
                    _0: "  y-->x=",
                    _1: {
                      TAG: /* Bool */9,
                      _0: /* No_padding */0,
                      _1: {
                        TAG: /* Formatting_lit */17,
                        _0: /* Flush_newline */4,
                        _1: /* End_of_format */0
                      }
                    }
                  }
                }
              },
              _1: "x-->y=%b  y-->x=%b@."
            }), Curry._2(path, vx, vy), Curry._2(path, vy, vx));
    if (!(Caml_array.get(num, x) > 0 && Caml_array.get(num, y) > 0)) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "test_topsort.ml",
              27,
              4
            ],
            Error: new Error()
          };
    }
    if (Caml_array.get(num, x) >= Caml_array.get(num, y) || Curry._2(path, vx, vy) || !Curry._2(path, vy, vx)) {
      return ;
    }
    throw {
          RE_EXN_ID: "Assert_failure",
          _1: [
            "test_topsort.ml",
            28,
            4
          ],
          Error: new Error()
        };
  };
  if (check) {
    for(var x = 0; x < n; ++x){
      for(var y = 0; y < n; ++y){
        check_edge([
              x,
              y
            ]);
      }
    }
  }
  
}

function tests(iter) {
  var test$1 = function (param, param$1) {
    return test(undefined, iter, param, param$1);
  };
  test$1(3, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            2
          ],
          tl: /* [] */0
        }
      });
  test$1(3, /* [] */0);
  test$1(1, {
        hd: [
          0,
          0
        ],
        tl: /* [] */0
      });
  test$1(2, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: /* [] */0
        }
      });
  test$1(3, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: {
            hd: [
              1,
              2
            ],
            tl: /* [] */0
          }
        }
      });
  test$1(3, {
        hd: [
          2,
          0
        ],
        tl: {
          hd: [
            0,
            2
          ],
          tl: {
            hd: [
              0,
              1
            ],
            tl: /* [] */0
          }
        }
      });
  test$1(3, {
        hd: [
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1
          ],
          tl: {
            hd: [
              2,
              0
            ],
            tl: /* [] */0
          }
        }
      });
  test$1(5, {
        hd: [
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1
          ],
          tl: {
            hd: [
              2,
              0
            ],
            tl: {
              hd: [
                3,
                4
              ],
              tl: {
                hd: [
                  4,
                  3
                ],
                tl: /* [] */0
              }
            }
          }
        }
      });
  test$1(3, {
        hd: [
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1
          ],
          tl: {
            hd: [
              0,
              2
            ],
            tl: /* [] */0
          }
        }
      });
  test$1(3, {
        hd: [
          1,
          2
        ],
        tl: {
          hd: [
            2,
            1
          ],
          tl: {
            hd: [
              0,
              1
            ],
            tl: /* [] */0
          }
        }
      });
  test$1(4, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: {
            hd: [
              2,
              3
            ],
            tl: {
              hd: [
                3,
                2
              ],
              tl: {
                hd: [
                  2,
                  1
                ],
                tl: /* [] */0
              }
            }
          }
        }
      });
  test$1(4, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: {
            hd: [
              2,
              3
            ],
            tl: {
              hd: [
                3,
                2
              ],
              tl: {
                hd: [
                  1,
                  2
                ],
                tl: /* [] */0
              }
            }
          }
        }
      });
  test$1(4, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: {
            hd: [
              2,
              3
            ],
            tl: {
              hd: [
                3,
                2
              ],
              tl: {
                hd: [
                  1,
                  2
                ],
                tl: {
                  hd: [
                    2,
                    1
                  ],
                  tl: /* [] */0
                }
              }
            }
          }
        }
      });
  test$1(5, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            2
          ],
          tl: {
            hd: [
              2,
              0
            ],
            tl: {
              hd: [
                3,
                0
              ],
              tl: {
                hd: [
                  2,
                  4
                ],
                tl: /* [] */0
              }
            }
          }
        }
      });
  test$1(7, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: {
            hd: [
              1,
              2
            ],
            tl: {
              hd: [
                2,
                3
              ],
              tl: {
                hd: [
                  3,
                  2
                ],
                tl: {
                  hd: [
                    3,
                    4
                  ],
                  tl: {
                    hd: [
                      4,
                      5
                    ],
                    tl: {
                      hd: [
                        5,
                        6
                      ],
                      tl: {
                        hd: [
                          6,
                          4
                        ],
                        tl: /* [] */0
                      }
                    }
                  }
                }
              }
            }
          }
        }
      });
  test$1(7, {
        hd: [
          0,
          1
        ],
        tl: {
          hd: [
            1,
            0
          ],
          tl: {
            hd: [
              1,
              2
            ],
            tl: {
              hd: [
                2,
                3
              ],
              tl: {
                hd: [
                  3,
                  2
                ],
                tl: {
                  hd: [
                    3,
                    4
                  ],
                  tl: {
                    hd: [
                      4,
                      5
                    ],
                    tl: {
                      hd: [
                        5,
                        6
                      ],
                      tl: {
                        hd: [
                          6,
                          4
                        ],
                        tl: {
                          hd: [
                            5,
                            2
                          ],
                          tl: /* [] */0
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      });
  return Format.printf(/* Format */{
              _0: {
                TAG: /* String_literal */11,
                _0: "All tests succeeded.",
                _1: {
                  TAG: /* Formatting_lit */17,
                  _0: /* Flush_newline */4,
                  _1: /* End_of_format */0
                }
              },
              _1: "All tests succeeded.@."
            });
}

tests(Pack.Digraph.Topological.iter);

var n = Caml_format.caml_int_of_string(Caml_array.get(Sys.argv, 1));

var el = /* [] */0;

el = {
  hd: [
    n - 1 | 0,
    0
  ],
  tl: /* [] */0
};

for(var i = 0 ,i_finish = n - 2 | 0; i <= i_finish; ++i){
  el = {
    hd: [
      i,
      i + 1 | 0
    ],
    tl: el
  };
}

test(false, Pack.Digraph.Topological.iter, n, el);

export {
  test ,
  tests ,
  n ,
  
}
/*  Not a pure module */
