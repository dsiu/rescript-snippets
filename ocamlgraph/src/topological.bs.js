// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Set from "rescript/lib/es6/set.js";
import * as List from "rescript/lib/es6/list.js";
import * as Path from "./path.bs.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Queue from "rescript/lib/es6/queue.js";
import * as Hashtbl from "rescript/lib/es6/hashtbl.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Caml_array from "rescript/lib/es6/caml_array.js";
import * as Components from "./components.bs.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function Make(funarg) {
  var Scc = Components.Make(funarg);
  var fold = function (f, g, acc) {
    var match = Curry._1(Scc.scc, g);
    var scc = match[1];
    var n = match[0];
    var vertices = Caml_array.make(n, /* [] */0);
    var edges = Caml_array.make(n, /* [] */0);
    var degree = Caml_array.make(n, 0);
    var add_vertex = function (x) {
      var ix = Curry._1(scc, x);
      Caml_array.set(vertices, ix, {
            hd: x,
            tl: Caml_array.get(vertices, ix)
          });
      var add_edge = function (y) {
        var iy = Curry._1(scc, y);
        if (ix !== iy) {
          Caml_array.set(edges, ix, {
                hd: iy,
                tl: Caml_array.get(edges, ix)
              });
          return Caml_array.set(degree, iy, Caml_array.get(degree, iy) + 1 | 0);
        }
        
      };
      return Curry._3(funarg.iter_succ, add_edge, g, x);
    };
    Curry._2(funarg.iter_vertex, add_vertex, g);
    var todo = Queue.create(undefined);
    for(var i = 0; i < n; ++i){
      if (Caml_array.get(degree, i) === 0) {
        Queue.push(i, todo);
      }
      
    }
    var _acc = acc;
    while(true) {
      var acc$1 = _acc;
      if (Queue.is_empty(todo)) {
        return acc$1;
      }
      var i$1 = Queue.pop(todo);
      var acc$2 = List.fold_right(f, Caml_array.get(vertices, i$1), acc$1);
      List.iter((function (j) {
              var d = Caml_array.get(degree, j);
              if (d <= 0) {
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: [
                        "topological.ml",
                        59,
                        13
                      ],
                      Error: new Error()
                    };
              }
              if (d === 1) {
                return Queue.push(j, todo);
              } else {
                return Caml_array.set(degree, j, d - 1 | 0);
              }
            }), Caml_array.get(edges, i$1));
      _acc = acc$2;
      continue ;
    };
  };
  var iter = function (f, g) {
    return fold((function (v, param) {
                  return Curry._1(f, v);
                }), g, undefined);
  };
  return {
          fold: fold,
          iter: iter
        };
}

function Make_stable(funarg) {
  var $$let = funarg.V;
  var H = Hashtbl.Make({
        equal: $$let.equal,
        hash: $$let.hash
      });
  var C = Path.Check({
        V: funarg.V,
        iter_succ: funarg.iter_succ
      });
  var choose = function (old, param) {
    var min = old[1];
    var n = param[1];
    var v = param[0];
    if (Caml_obj.caml_equal(n, min)) {
      return [
              {
                hd: v,
                tl: old[0]
              },
              n
            ];
    } else if (Caml_obj.caml_lessthan(n, min)) {
      return [
              {
                hd: v,
                tl: /* [] */0
              },
              n
            ];
    } else {
      return old;
    }
  };
  var S = $$Set.Make(funarg.V);
  var push = function (v, s) {
    s.contents = Curry._2(S.add, v, s.contents);
    
  };
  var pop = function (s) {
    var r = Curry._1(S.min_elt, s.contents);
    s.contents = Curry._2(S.remove, r, s.contents);
    return r;
  };
  var find_top_cycle = function (checker, vl) {
    var on_top_cycle = function (v) {
      return List.for_all((function (v$p) {
                    if (Curry._2(funarg.V.equal, v, v$p) || Curry._3(C.check_path, checker, v, v$p)) {
                      return true;
                    } else {
                      return !Curry._3(C.check_path, checker, v$p, v);
                    }
                  }), vl);
    };
    return List.filter(on_top_cycle)(vl);
  };
  var fold = function (f, g, acc) {
    var checker = Curry._1(C.create, g);
    var degree = Curry._1(H.create, 97);
    var todo = {
      contents: S.empty
    };
    var push$1 = function (x) {
      Curry._2(H.remove, degree, x);
      return push(x, todo);
    };
    Curry._2(funarg.iter_vertex, (function (v) {
            var d = Curry._2(funarg.in_degree, g, v);
            if (d === 0) {
              return push(v, todo);
            } else {
              return Curry._3(H.add, degree, v, d);
            }
          }), g);
    var _acc = acc;
    while(true) {
      var acc$1 = _acc;
      if (Curry._1(S.is_empty, todo.contents)) {
        var match = Curry._3(H.fold, (function (v, d, old) {
                var new_ = [
                  v,
                  d
                ];
                var match = choose(old, new_);
                return [
                        List.sort(funarg.V.compare, match[0]),
                        match[1]
                      ];
              }), degree, [
              /* [] */0,
              Pervasives.max_int
            ]);
        var min = match[0];
        if (!min) {
          return acc$1;
        }
        var vl = find_top_cycle(checker, min);
        List.iter(push$1, vl);
        continue ;
      }
      var v = pop(todo);
      var acc$2 = Curry._2(f, v, acc$1);
      Curry._3(funarg.iter_succ, (function (x) {
              try {
                var d = Curry._2(H.find, degree, x);
                if (d === 1) {
                  return push$1(x);
                } else {
                  return Curry._3(H.replace, degree, x, d - 1 | 0);
                }
              }
              catch (raw_exn){
                var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                if (exn.RE_EXN_ID === "Not_found") {
                  return ;
                }
                throw exn;
              }
            }), g, v);
      _acc = acc$2;
      continue ;
    };
  };
  var iter = function (f, g) {
    return fold((function (v, param) {
                  return Curry._1(f, v);
                }), g, undefined);
  };
  return {
          fold: fold,
          iter: iter
        };
}

export {
  Make ,
  Make_stable ,
  
}
/* No side effect */
