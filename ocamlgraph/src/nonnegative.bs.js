// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Map from "rescript/lib/es6/map.js";
import * as $$Set from "rescript/lib/es6/set.js";
import * as List from "rescript/lib/es6/list.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Queue from "rescript/lib/es6/queue.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";
import * as Caml_option from "rescript/lib/es6/caml_option.js";
import * as Caml_exceptions from "rescript/lib/es6/caml_exceptions.js";
import * as Caml_js_exceptions from "rescript/lib/es6/caml_js_exceptions.js";

function Imperative(funarg, funarg$1) {
  var S = $$Set.Make(funarg.V);
  var M = $$Map.Make(funarg.V);
  var V = funarg.V;
  var E = funarg.E;
  var dump_cycle = function (cycle) {
    var v0 = Curry._1(funarg.E.src, List.hd(cycle));
    Pervasives.print_string("(" + (String(Curry._1(V.label, v0)) + ")"));
    var v1 = List.fold_left((function (v, e) {
            if (Curry._2(funarg.V.compare, v, Curry._1(funarg.E.src, e)) !== 0) {
              throw {
                    RE_EXN_ID: "Assert_failure",
                    _1: [
                      "nonnegative.ml",
                      44,
                      8
                    ],
                    Error: new Error()
                  };
            }
            var v$1 = Curry._1(funarg.E.dst, e);
            Pervasives.print_string("-(" + (String(Curry._1(V.label, v$1)) + ")"));
            return v$1;
          }), v0, cycle);
    if (!Caml_obj.caml_equal(v0, v1)) {
      throw {
            RE_EXN_ID: "Assert_failure",
            _1: [
              "nonnegative.ml",
              48,
              4
            ],
            Error: new Error()
          };
    }
    return Pervasives.print_string("\n");
  };
  var dump_set = Curry._1(S.iter, (function (x) {
          return Pervasives.print_string(String(Curry._1(V.label, x)) + ", ");
        }));
  var dump = function (param) {
    Pervasives.print_string("====================\nS: ");
    Curry._1(dump_set, param[0].contents);
    Pervasives.print_string("\nMap:");
    Curry._2(M.iter, (function (k, v) {
            Pervasives.print_string("\n  " + (String(Curry._1(V.label, k)) + ": "));
            return Curry._2(M.iter, (function (k, param) {
                          var origin = param[0];
                          var tmp;
                          if (origin !== undefined) {
                            var e = Caml_option.valFromOption(origin);
                            var v = Curry._1(funarg.E.src, e);
                            var v$1 = Curry._1(funarg.E.dst, e);
                            tmp = String(Curry._1(V.label, v)) + (">" + String(Curry._1(V.label, v$1)));
                          } else {
                            tmp = "---";
                          }
                          return Pervasives.print_string("(" + (String(Curry._1(V.label, k)) + (">>" + (tmp + (":" + (String(param[1]) + ") "))))));
                        }), v);
          }), param[1].contents);
    return Pervasives.print_string("\n");
  };
  var Negative_cycle = /* @__PURE__ */Caml_exceptions.create("Nonnegative.Imperative(G)(W).Negative_cycle");
  var create = function (size, param) {
    var g = size !== undefined ? Curry._2(funarg.create, size, undefined) : Curry._2(funarg.create, undefined, undefined);
    return [
            g,
            {
              contents: S.empty
            },
            {
              contents: M.empty
            }
          ];
  };
  var copy = function (param) {
    return [
            Curry._1(funarg.copy, param[0]),
            {
              contents: param[1].contents
            },
            {
              contents: param[2].contents
            }
          ];
  };
  var clear = function (param) {
    Curry._1(funarg.clear, param[0]);
    param[1].contents = S.empty;
    param[2].contents = M.empty;
    
  };
  var add_vertex = function (param, v) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (!Curry._2(funarg.mem_vertex, g, v)) {
      Curry._2(funarg.add_vertex, g, v);
      src.contents = Curry._2(S.add, v, src.contents);
      dist.contents = Curry._3(M.add, v, Curry._3(M.add, v, [
                undefined,
                funarg$1.zero
              ], M.empty), dist.contents);
      return dump([
                  src,
                  dist
                ]);
    }
    
  };
  var propagate = function (_param, q, start) {
    while(true) {
      var param = _param;
      var dist = param[2];
      var src = param[1];
      var g = param[0];
      if (Queue.is_empty(q)) {
        return [
                g,
                src,
                dist
              ];
      }
      var match = Queue.pop(q);
      var v1src = match[1];
      var v1 = match[0];
      var v1dist = Curry._2(M.find, v1, dist);
      var dist$1 = Curry._4(funarg.fold_succ_e, (function(v1src,v1dist){
          return function (e, dist) {
            var v2 = Curry._1(funarg.E.dst, e);
            var v2dist = Curry._2(M.mem, v2, dist) ? Curry._2(M.find, v2, dist) : M.empty;
            var match = Curry._3(S.fold, (function (x, param) {
                    var nextSrc = param[1];
                    var v2dist = param[0];
                    var match = Curry._2(M.find, x, v1dist);
                    var ndev2 = Curry._2(funarg$1.add, match[1], Curry._1(funarg$1.weight, e));
                    var improvement;
                    try {
                      var match$1 = Curry._2(M.find, x, v2dist);
                      improvement = Curry._2(funarg$1.compare, ndev2, match$1[1]) < 0;
                    }
                    catch (raw_exn){
                      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                      if (exn.RE_EXN_ID === "Not_found") {
                        improvement = true;
                      } else {
                        throw exn;
                      }
                    }
                    if (!improvement) {
                      return [
                              v2dist,
                              nextSrc
                            ];
                    }
                    var v2dist$1 = Curry._3(M.add, x, [
                          Caml_option.some(e),
                          ndev2
                        ], v2dist);
                    var nextSrc$1 = Curry._2(S.add, x, nextSrc);
                    return [
                            v2dist$1,
                            nextSrc$1
                          ];
                  }), v1src, [
                  v2dist,
                  S.empty
                ]);
            var nextSrc = match[1];
            var v2dist$1 = match[0];
            if (Curry._1(S.is_empty, nextSrc)) {
              return dist;
            }
            if (Curry._2(funarg.V.equal, start, v2)) {
              var dist$1 = Curry._3(M.add, v2, v2dist$1, dist);
              var cycle = Curry._3(S.fold, (function (s, x) {
                      if (x !== undefined) {
                        return x;
                      } else {
                        var _x = v2;
                        var _ret = /* [] */0;
                        while(true) {
                          var ret = _ret;
                          var x$1 = _x;
                          var match = Curry._2(M.find, s, Curry._2(M.find, x$1, dist$1));
                          var e = match[0];
                          if (e === undefined) {
                            return ;
                          }
                          var e$1 = Caml_option.valFromOption(e);
                          var y = Curry._1(funarg.E.src, e$1);
                          var cycle = {
                            hd: e$1,
                            tl: ret
                          };
                          if (Curry._2(funarg.V.equal, start, y)) {
                            return cycle;
                          }
                          _ret = cycle;
                          _x = y;
                          continue ;
                        };
                      }
                    }), nextSrc, undefined);
              var cycle$1;
              if (cycle !== undefined) {
                cycle$1 = cycle;
              } else {
                throw {
                      RE_EXN_ID: "Assert_failure",
                      _1: [
                        "nonnegative.ml",
                        152,
                        38
                      ],
                      Error: new Error()
                    };
              }
              dump_cycle(cycle$1);
              throw {
                    RE_EXN_ID: Negative_cycle,
                    _1: cycle$1,
                    Error: new Error()
                  };
            }
            Queue.push([
                  v2,
                  nextSrc
                ], q);
            return Curry._3(M.add, v2, v2dist$1, dist);
          }
          }(v1src,v1dist)), g, v1, dist);
      _param = [
        g,
        src,
        dist$1
      ];
      continue ;
    };
  };
  var m_cardinal = function (m) {
    return Curry._3(M.fold, (function (param, param$1, acc) {
                  return acc + 1 | 0;
                }), m, 0);
  };
  var set_of_map = function (m) {
    return Curry._3(M.fold, (function (k, param, acc) {
                  return Curry._2(S.add, k, acc);
                }), m, S.empty);
  };
  var add_edge_internal = function (param, v1, v2) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    var dv1 = Curry._2(M.find, v1, dist);
    var q = Queue.create(undefined);
    if (m_cardinal(dv1) === 1 && Curry._2(M.mem, v2, dv1)) {
      Queue.add([
            v1,
            Curry._2(S.add, v2, S.empty)
          ], q);
      return propagate([
                  g,
                  src,
                  dist
                ], q, v1);
    }
    var match;
    if (Curry._2(S.mem, v2, src)) {
      var src$1 = Curry._2(S.remove, v2, src);
      var dist$1 = Curry._2(M.map, Curry._1(M.remove, v2), dist);
      var dv1$1 = Curry._2(M.find, v1, dist$1);
      match = [
        src$1,
        dist$1,
        dv1$1
      ];
    } else {
      match = [
        src,
        dist,
        dv1
      ];
    }
    Queue.add([
          v1,
          set_of_map(match[2])
        ], q);
    return propagate([
                g,
                match[0],
                match[1]
              ], q, v1);
  };
  var add_edge_e = function (param, e) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (Curry._2(funarg.mem_edge_e, g, e)) {
      return ;
    }
    var v1 = Curry._1(funarg.E.src, e);
    var v2 = Curry._1(funarg.E.dst, e);
    var partial_arg = [
      g,
      src,
      dist
    ];
    List.iter((function (param) {
            return add_vertex(partial_arg, param);
          }), {
          hd: v1,
          tl: {
            hd: v2,
            tl: /* [] */0
          }
        });
    try {
      Curry._2(funarg.add_edge_e, g, e);
      var match = add_edge_internal([
            g,
            src.contents,
            dist.contents
          ], v1, v2);
      src.contents = match[1];
      dist.contents = match[2];
    }
    catch (exp){
      Curry._2(funarg.remove_edge_e, g, e);
      throw exp;
    }
    return dump([
                src,
                dist
              ]);
  };
  var add_edge = function (param, v1, v2) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (Curry._3(funarg.mem_edge, g, v1, v2)) {
      return ;
    }
    var partial_arg = [
      g,
      src,
      dist
    ];
    List.iter((function (param) {
            return add_vertex(partial_arg, param);
          }), {
          hd: v1,
          tl: {
            hd: v2,
            tl: /* [] */0
          }
        });
    try {
      Curry._3(funarg.add_edge, g, v1, v2);
      var match = add_edge_internal([
            g,
            src.contents,
            dist.contents
          ], v1, v2);
      src.contents = match[1];
      dist.contents = match[2];
    }
    catch (exp){
      Curry._3(funarg.remove_edge, g, v1, v2);
      throw exp;
    }
    return dump([
                src,
                dist
              ]);
  };
  var remove_edge_internal = function (param, v2) {
    var src = param[1];
    var q = Queue.create(undefined);
    Pervasives.print_string("dump: ");
    Curry._1(dump_set, src);
    var dist = Curry._3(S.fold, (function (x, dist) {
            Pervasives.print_string("source: " + (String(Curry._1(V.label, x)) + "\n"));
            Queue.add([
                  x,
                  Curry._2(S.add, x, S.empty)
                ], q);
            return Curry._3(M.add, x, Curry._3(M.add, x, [
                            undefined,
                            funarg$1.zero
                          ], M.empty), dist);
          }), src, M.empty);
    var match = propagate([
          param[0],
          src,
          dist
        ], q, Curry._1(S.choose, src));
    var dist$1 = match[2];
    var src$1 = match[1];
    var g = match[0];
    if (Curry._2(M.mem, v2, dist$1)) {
      return [
              g,
              src$1,
              dist$1
            ];
    }
    Queue.add([
          v2,
          Curry._2(S.add, v2, S.empty)
        ], q);
    var src$2 = Curry._2(S.add, v2, src$1);
    var dist$2 = Curry._3(M.add, v2, Curry._3(M.add, v2, [
              undefined,
              funarg$1.zero
            ], M.empty), dist$1);
    return propagate([
                g,
                src$2,
                dist$2
              ], q, v2);
  };
  var remove_edge_e = function (param, e) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (!Curry._2(funarg.mem_edge_e, g, e)) {
      return ;
    }
    Curry._2(funarg.remove_edge_e, g, e);
    var v2 = Curry._1(funarg.E.dst, e);
    var match = remove_edge_internal([
          g,
          src.contents
        ], v2);
    src.contents = match[1];
    dist.contents = match[2];
    return dump([
                src,
                dist
              ]);
  };
  var remove_edge = function (param, v1, v2) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (!Curry._3(funarg.mem_edge, g, v1, v2)) {
      return ;
    }
    Curry._3(funarg.remove_edge, g, v1, v2);
    var match = remove_edge_internal([
          g,
          src.contents
        ], v2);
    src.contents = match[1];
    dist.contents = match[2];
    return dump([
                src,
                dist
              ]);
  };
  var remove_vertex = function (param, v) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (Curry._2(funarg.mem_vertex, g, v)) {
      Curry._3(funarg.iter_succ_e, (function (e) {
              return remove_edge_e([
                          g,
                          src,
                          dist
                        ], e);
            }), g, v);
      Curry._3(funarg.iter_pred_e, (function (e) {
              return remove_edge_e([
                          g,
                          src,
                          dist
                        ], e);
            }), g, v);
      Curry._2(funarg.remove_vertex, g, v);
      src.contents = Curry._2(S.remove, v, src.contents);
      dist.contents = Curry._2(M.remove, v, Curry._2(M.map, Curry._1(M.remove, v), dist.contents));
      return dump([
                  src,
                  dist
                ]);
    }
    
  };
  var map_vertex = function (f, param) {
    var map_map = function (update, m) {
      return Curry._3(M.fold, (function (v, m, acc) {
                    return Curry._3(M.add, Curry._1(f, v), Curry._1(update, m), acc);
                  }), m, M.empty);
    };
    var g = Curry._2(funarg.map_vertex, f, param[0]);
    var src = Curry._3(S.fold, (function (v, acc) {
            return Curry._2(S.add, Curry._1(f, v), acc);
          }), param[1].contents, S.empty);
    var update = function (v) {
      var e = v[0];
      if (e === undefined) {
        return v;
      }
      var e$1 = Caml_option.valFromOption(e);
      return [
              Caml_option.some(Curry._3(E.create, Curry._1(f, Curry._1(E.src, e$1)), Curry._1(E.label, e$1), Curry._1(f, Curry._1(E.dst, e$1)))),
              v[1]
            ];
    };
    var dist = map_map((function (param) {
            return map_map(update, param);
          }), param[2].contents);
    return [
            g,
            {
              contents: src
            },
            {
              contents: dist
            }
          ];
  };
  var fold_pred_e = function (f, param) {
    return Curry._2(funarg.fold_pred_e, f, param[0]);
  };
  var iter_pred_e = function (f, param) {
    return Curry._2(funarg.iter_pred_e, f, param[0]);
  };
  var fold_succ_e = function (f, param) {
    return Curry._2(funarg.fold_succ_e, f, param[0]);
  };
  var iter_succ_e = function (f, param) {
    return Curry._2(funarg.iter_succ_e, f, param[0]);
  };
  var fold_pred = function (f, param) {
    return Curry._2(funarg.fold_pred, f, param[0]);
  };
  var fold_succ = function (f, param) {
    return Curry._2(funarg.fold_succ, f, param[0]);
  };
  var iter_pred = function (f, param) {
    return Curry._2(funarg.iter_pred, f, param[0]);
  };
  var iter_succ = function (f, param) {
    return Curry._2(funarg.iter_succ, f, param[0]);
  };
  var fold_edges_e = function (f, param) {
    return Curry._2(funarg.fold_edges_e, f, param[0]);
  };
  var iter_edges_e = function (f, param) {
    return Curry._2(funarg.iter_edges_e, f, param[0]);
  };
  var fold_edges = function (f, param) {
    return Curry._2(funarg.fold_edges, f, param[0]);
  };
  var iter_edges = function (f, param) {
    return Curry._2(funarg.iter_edges, f, param[0]);
  };
  var fold_vertex = function (f, param) {
    return Curry._2(funarg.fold_vertex, f, param[0]);
  };
  var iter_vertex = function (f, param) {
    return Curry._2(funarg.iter_vertex, f, param[0]);
  };
  var pred_e = function (param) {
    return Curry._1(funarg.pred_e, param[0]);
  };
  var succ_e = function (param) {
    return Curry._1(funarg.succ_e, param[0]);
  };
  var pred = function (param) {
    return Curry._1(funarg.pred, param[0]);
  };
  var succ = function (param) {
    return Curry._1(funarg.succ, param[0]);
  };
  var find_all_edges = function (param) {
    return Curry._1(funarg.find_all_edges, param[0]);
  };
  var find_edge = function (param) {
    return Curry._1(funarg.find_edge, param[0]);
  };
  var mem_edge_e = function (param) {
    return Curry._1(funarg.mem_edge_e, param[0]);
  };
  var mem_edge = function (param) {
    return Curry._1(funarg.mem_edge, param[0]);
  };
  var mem_vertex = function (param) {
    return Curry._1(funarg.mem_vertex, param[0]);
  };
  var in_degree = function (param) {
    return Curry._1(funarg.in_degree, param[0]);
  };
  var out_degree = function (param) {
    return Curry._1(funarg.out_degree, param[0]);
  };
  var nb_edges = function (param) {
    return Curry._1(funarg.nb_edges, param[0]);
  };
  var nb_vertex = function (param) {
    return Curry._1(funarg.nb_vertex, param[0]);
  };
  var is_empty = function (param) {
    return Curry._1(funarg.is_empty, param[0]);
  };
  var clear$1 = function (g) {
    return Curry._1(funarg.Mark.clear, g[0]);
  };
  var get = funarg.Mark.get;
  var set = funarg.Mark.set;
  var Mark = {
    clear: clear$1,
    get: get,
    set: set
  };
  return {
          V: V,
          E: E,
          is_directed: funarg.is_directed,
          is_empty: is_empty,
          nb_vertex: nb_vertex,
          nb_edges: nb_edges,
          out_degree: out_degree,
          in_degree: in_degree,
          mem_vertex: mem_vertex,
          mem_edge: mem_edge,
          mem_edge_e: mem_edge_e,
          find_edge: find_edge,
          find_all_edges: find_all_edges,
          succ: succ,
          pred: pred,
          succ_e: succ_e,
          pred_e: pred_e,
          iter_vertex: iter_vertex,
          fold_vertex: fold_vertex,
          iter_edges: iter_edges,
          fold_edges: fold_edges,
          iter_edges_e: iter_edges_e,
          fold_edges_e: fold_edges_e,
          map_vertex: map_vertex,
          iter_succ: iter_succ,
          iter_pred: iter_pred,
          fold_succ: fold_succ,
          fold_pred: fold_pred,
          iter_succ_e: iter_succ_e,
          fold_succ_e: fold_succ_e,
          iter_pred_e: iter_pred_e,
          fold_pred_e: fold_pred_e,
          create: create,
          clear: clear,
          copy: copy,
          add_vertex: add_vertex,
          remove_vertex: remove_vertex,
          add_edge: add_edge,
          add_edge_e: add_edge_e,
          remove_edge: remove_edge,
          remove_edge_e: remove_edge_e,
          Mark: Mark,
          Negative_cycle: Negative_cycle
        };
}

function Persistent(funarg, funarg$1) {
  var S = $$Set.Make(funarg.V);
  var M = $$Map.Make(funarg.V);
  var E = funarg.E;
  var Negative_cycle = /* @__PURE__ */Caml_exceptions.create("Nonnegative.Persistent(G)(W).Negative_cycle");
  var empty_0 = funarg.empty;
  var empty_1 = S.empty;
  var empty_2 = M.empty;
  var empty = [
    empty_0,
    empty_1,
    empty_2
  ];
  var add_vertex = function (param, v) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (Curry._2(funarg.mem_vertex, g, v)) {
      return [
              g,
              src,
              dist
            ];
    } else {
      return [
              Curry._2(funarg.add_vertex, g, v),
              Curry._2(S.add, v, src),
              Curry._3(M.add, v, Curry._3(M.add, v, [
                        undefined,
                        funarg$1.zero
                      ], M.empty), dist)
            ];
    }
  };
  var propagate = function (_param, q, start) {
    while(true) {
      var param = _param;
      var dist = param[2];
      var src = param[1];
      var g = param[0];
      if (Queue.is_empty(q)) {
        return [
                g,
                src,
                dist
              ];
      }
      var match = Queue.pop(q);
      var v1src = match[1];
      var v1 = match[0];
      var v1dist = Curry._2(M.find, v1, dist);
      var dist$1 = Curry._4(funarg.fold_succ_e, (function(v1src,v1dist){
          return function (e, dist) {
            var v2 = Curry._1(funarg.E.dst, e);
            var v2dist = Curry._2(M.find, v2, dist);
            var match = Curry._3(S.fold, (function (x, param) {
                    var nextSrc = param[1];
                    var v2dist = param[0];
                    var match = Curry._2(M.find, x, v1dist);
                    var ndev2 = Curry._2(funarg$1.add, match[1], Curry._1(funarg$1.weight, e));
                    var improvement;
                    try {
                      var match$1 = Curry._2(M.find, x, v2dist);
                      improvement = Curry._2(funarg$1.compare, ndev2, match$1[1]) < 0;
                    }
                    catch (raw_exn){
                      var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
                      if (exn.RE_EXN_ID === "Not_found") {
                        improvement = true;
                      } else {
                        throw exn;
                      }
                    }
                    if (!improvement) {
                      return [
                              v2dist,
                              nextSrc
                            ];
                    }
                    var v2dist$1 = Curry._3(M.add, x, [
                          Caml_option.some(e),
                          ndev2
                        ], v2dist);
                    var nextSrc$1 = Curry._2(S.add, x, nextSrc);
                    return [
                            v2dist$1,
                            nextSrc$1
                          ];
                  }), v1src, [
                  v2dist,
                  S.empty
                ]);
            var nextSrc = match[1];
            if (Curry._1(S.is_empty, nextSrc)) {
              return dist;
            }
            if (Curry._2(funarg.V.equal, start, v2)) {
              var s = Curry._1(S.choose, nextSrc);
              var build_cycle = function (_x, _ret) {
                while(true) {
                  var ret = _ret;
                  var x = _x;
                  var match = Curry._2(M.find, s, Curry._2(M.find, x, dist));
                  var e = match[0];
                  if (e !== undefined) {
                    var e$1 = Caml_option.valFromOption(e);
                    var y = Curry._1(funarg.E.src, e$1);
                    var cycle = {
                      hd: e$1,
                      tl: ret
                    };
                    if (Curry._2(funarg.V.equal, start, y)) {
                      return cycle;
                    }
                    _ret = cycle;
                    _x = y;
                    continue ;
                  }
                  throw {
                        RE_EXN_ID: "Assert_failure",
                        _1: [
                          "nonnegative.ml",
                          481,
                          21
                        ],
                        Error: new Error()
                      };
                };
              };
              throw {
                    RE_EXN_ID: Negative_cycle,
                    _1: build_cycle(v2, /* [] */0),
                    Error: new Error()
                  };
            }
            Queue.push([
                  v2,
                  nextSrc
                ], q);
            return Curry._3(M.add, v2, match[0], dist);
          }
          }(v1src,v1dist)), g, v1, dist);
      _param = [
        g,
        src,
        dist$1
      ];
      continue ;
    };
  };
  var m_cardinal = function (m) {
    return Curry._3(M.fold, (function (param, param$1, acc) {
                  return acc + 1 | 0;
                }), m, 0);
  };
  var set_of_map = function (m) {
    return Curry._3(M.fold, (function (k, param, acc) {
                  return Curry._2(S.add, k, acc);
                }), m, S.empty);
  };
  var add_edge_internal = function (param, v1, v2) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    var dv1 = Curry._2(M.find, v1, dist);
    var q = Queue.create(undefined);
    if (m_cardinal(dv1) === 1 && Curry._2(M.mem, v2, dv1)) {
      Queue.add([
            v1,
            Curry._2(S.add, v2, S.empty)
          ], q);
      return propagate([
                  g,
                  src,
                  dist
                ], q, v1);
    }
    var match = Curry._2(S.mem, v2, src) ? [
        Curry._2(S.remove, v2, src),
        Curry._2(M.map, Curry._1(M.remove, v2), dist),
        Curry._2(M.find, v1, dist)
      ] : [
        src,
        dist,
        dv1
      ];
    Queue.add([
          v1,
          set_of_map(match[2])
        ], q);
    return propagate([
                g,
                match[0],
                match[1]
              ], q, v1);
  };
  var add_edge_e = function (param, e) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (Curry._2(funarg.mem_edge_e, g, e)) {
      return [
              g,
              src,
              dist
            ];
    }
    var v1 = Curry._1(funarg.E.src, e);
    var v2 = Curry._1(funarg.E.dst, e);
    var match = List.fold_left(add_vertex, [
          g,
          src,
          dist
        ], {
          hd: v1,
          tl: {
            hd: v2,
            tl: /* [] */0
          }
        });
    var g$1 = Curry._2(funarg.add_edge_e, match[0], e);
    return add_edge_internal([
                g$1,
                match[1],
                match[2]
              ], v1, v2);
  };
  var add_edge = function (param, v1, v2) {
    var dist = param[2];
    var src = param[1];
    var g = param[0];
    if (Curry._3(funarg.mem_edge, g, v1, v2)) {
      return [
              g,
              src,
              dist
            ];
    }
    var match = List.fold_left(add_vertex, [
          g,
          src,
          dist
        ], {
          hd: v1,
          tl: {
            hd: v2,
            tl: /* [] */0
          }
        });
    var g$1 = Curry._3(funarg.add_edge, match[0], v1, v2);
    return add_edge_internal([
                g$1,
                match[1],
                match[2]
              ], v1, v2);
  };
  var remove_edge_internal = function (param, v2) {
    var src = param[1];
    var q = Queue.create(undefined);
    var dist = Curry._3(S.fold, (function (x, dist) {
            Queue.add([
                  x,
                  Curry._2(S.add, x, S.empty)
                ], q);
            return Curry._3(M.add, x, Curry._3(M.add, x, [
                            undefined,
                            funarg$1.zero
                          ], M.empty), dist);
          }), src, M.empty);
    var match = propagate([
          param[0],
          src,
          dist
        ], q, Curry._1(S.choose, src));
    var dist$1 = match[2];
    var src$1 = match[1];
    var g = match[0];
    if (Curry._2(M.mem, v2, dist$1)) {
      return [
              g,
              src$1,
              dist$1
            ];
    }
    Queue.add([
          v2,
          Curry._2(S.add, v2, S.empty)
        ], q);
    var src$2 = Curry._2(S.add, v2, src$1);
    var dist$2 = Curry._3(M.add, v2, Curry._3(M.add, v2, [
              undefined,
              funarg$1.zero
            ], M.empty), dist$1);
    return propagate([
                g,
                src$2,
                dist$2
              ], q, v2);
  };
  var remove_edge_e = function (param, e) {
    var src = param[1];
    var g = param[0];
    if (!Curry._2(funarg.mem_edge_e, g, e)) {
      return [
              g,
              src,
              param[2]
            ];
    }
    var g$1 = Curry._2(funarg.remove_edge_e, g, e);
    var v2 = Curry._1(funarg.E.dst, e);
    return remove_edge_internal([
                g$1,
                src
              ], v2);
  };
  var remove_edge = function (param, v1, v2) {
    var src = param[1];
    var g = param[0];
    if (!Curry._3(funarg.mem_edge, g, v1, v2)) {
      return [
              g,
              src,
              param[2]
            ];
    }
    var g$1 = Curry._3(funarg.remove_edge, g, v1, v2);
    return remove_edge_internal([
                g$1,
                src
              ], v2);
  };
  var remove_vertex = function (t, v) {
    var t$1 = Curry._4(funarg.fold_succ_e, (function (e, t) {
            return remove_edge_e(t, e);
          }), t[0], v, t);
    var t$2 = Curry._4(funarg.fold_pred_e, (function (e, t) {
            return remove_edge_e(t, e);
          }), t$1[0], v, t$1);
    return [
            Curry._2(funarg.remove_vertex, t$2[0], v),
            Curry._2(S.remove, v, t$2[1]),
            Curry._2(M.map, Curry._1(M.remove, v), t$2[2])
          ];
  };
  var map_vertex = function (f, param) {
    var map_map = function (update, m) {
      return Curry._3(M.fold, (function (v, m, acc) {
                    return Curry._3(M.add, Curry._1(f, v), Curry._1(update, m), acc);
                  }), m, M.empty);
    };
    var update = function (v) {
      var e = v[0];
      if (e === undefined) {
        return v;
      }
      var e$1 = Caml_option.valFromOption(e);
      return [
              Caml_option.some(Curry._3(E.create, Curry._1(f, Curry._1(E.src, e$1)), Curry._1(E.label, e$1), Curry._1(f, Curry._1(E.dst, e$1)))),
              v[1]
            ];
    };
    return [
            Curry._2(funarg.map_vertex, f, param[0]),
            Curry._3(S.fold, (function (v, acc) {
                    return Curry._2(S.add, Curry._1(f, v), acc);
                  }), param[1], S.empty),
            map_map((function (param) {
                    return map_map(update, param);
                  }), param[2])
          ];
  };
  var fold_pred_e = function (f, param) {
    return Curry._2(funarg.fold_pred_e, f, param[0]);
  };
  var iter_pred_e = function (f, param) {
    return Curry._2(funarg.iter_pred_e, f, param[0]);
  };
  var fold_succ_e = function (f, param) {
    return Curry._2(funarg.fold_succ_e, f, param[0]);
  };
  var iter_succ_e = function (f, param) {
    return Curry._2(funarg.iter_succ_e, f, param[0]);
  };
  var fold_pred = function (f, param) {
    return Curry._2(funarg.fold_pred, f, param[0]);
  };
  var fold_succ = function (f, param) {
    return Curry._2(funarg.fold_succ, f, param[0]);
  };
  var iter_pred = function (f, param) {
    return Curry._2(funarg.iter_pred, f, param[0]);
  };
  var iter_succ = function (f, param) {
    return Curry._2(funarg.iter_succ, f, param[0]);
  };
  var fold_edges_e = function (f, param) {
    return Curry._2(funarg.fold_edges_e, f, param[0]);
  };
  var iter_edges_e = function (f, param) {
    return Curry._2(funarg.iter_edges_e, f, param[0]);
  };
  var fold_edges = function (f, param) {
    return Curry._2(funarg.fold_edges, f, param[0]);
  };
  var iter_edges = function (f, param) {
    return Curry._2(funarg.iter_edges, f, param[0]);
  };
  var fold_vertex = function (f, param) {
    return Curry._2(funarg.fold_vertex, f, param[0]);
  };
  var iter_vertex = function (f, param) {
    return Curry._2(funarg.iter_vertex, f, param[0]);
  };
  var pred_e = function (param) {
    return Curry._1(funarg.pred_e, param[0]);
  };
  var succ_e = function (param) {
    return Curry._1(funarg.succ_e, param[0]);
  };
  var pred = function (param) {
    return Curry._1(funarg.pred, param[0]);
  };
  var succ = function (param) {
    return Curry._1(funarg.succ, param[0]);
  };
  var find_all_edges = function (param) {
    return Curry._1(funarg.find_all_edges, param[0]);
  };
  var find_edge = function (param) {
    return Curry._1(funarg.find_edge, param[0]);
  };
  var mem_edge_e = function (param) {
    return Curry._1(funarg.mem_edge_e, param[0]);
  };
  var mem_edge = function (param) {
    return Curry._1(funarg.mem_edge, param[0]);
  };
  var mem_vertex = function (param) {
    return Curry._1(funarg.mem_vertex, param[0]);
  };
  var in_degree = function (param) {
    return Curry._1(funarg.in_degree, param[0]);
  };
  var out_degree = function (param) {
    return Curry._1(funarg.out_degree, param[0]);
  };
  var nb_edges = function (param) {
    return Curry._1(funarg.nb_edges, param[0]);
  };
  var nb_vertex = function (param) {
    return Curry._1(funarg.nb_vertex, param[0]);
  };
  var is_empty = function (param) {
    return Curry._1(funarg.is_empty, param[0]);
  };
  return {
          V: funarg.V,
          E: E,
          is_directed: funarg.is_directed,
          is_empty: is_empty,
          nb_vertex: nb_vertex,
          nb_edges: nb_edges,
          out_degree: out_degree,
          in_degree: in_degree,
          mem_vertex: mem_vertex,
          mem_edge: mem_edge,
          mem_edge_e: mem_edge_e,
          find_edge: find_edge,
          find_all_edges: find_all_edges,
          succ: succ,
          pred: pred,
          succ_e: succ_e,
          pred_e: pred_e,
          iter_vertex: iter_vertex,
          fold_vertex: fold_vertex,
          iter_edges: iter_edges,
          fold_edges: fold_edges,
          iter_edges_e: iter_edges_e,
          fold_edges_e: fold_edges_e,
          map_vertex: map_vertex,
          iter_succ: iter_succ,
          iter_pred: iter_pred,
          fold_succ: fold_succ,
          fold_pred: fold_pred,
          iter_succ_e: iter_succ_e,
          fold_succ_e: fold_succ_e,
          iter_pred_e: iter_pred_e,
          fold_pred_e: fold_pred_e,
          empty: empty,
          add_vertex: add_vertex,
          remove_vertex: remove_vertex,
          add_edge: add_edge,
          add_edge_e: add_edge_e,
          remove_edge: remove_edge,
          remove_edge_e: remove_edge_e,
          Negative_cycle: Negative_cycle
        };
}

export {
  Imperative ,
  Persistent ,
  
}
/* No side effect */
