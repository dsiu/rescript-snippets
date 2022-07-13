// Generated by ReScript, PLEASE EDIT WITH CARE

import * as $$Set from "rescript/lib/es6/set.js";
import * as Gmap from "./gmap.bs.js";
import * as List from "rescript/lib/es6/list.js";
import * as Oper from "./oper.bs.js";
import * as Curry from "rescript/lib/es6/curry.js";
import * as Builder from "./builder.bs.js";
import * as Caml_obj from "rescript/lib/es6/caml_obj.js";
import * as Cliquetree from "./cliquetree.bs.js";
import * as Pervasives from "rescript/lib/es6/pervasives.js";

function P(funarg) {
  $$Set.Make(funarg.V);
  var CT = Cliquetree.CliqueTree(funarg);
  Oper.Choose({
        iter_vertex: funarg.iter_vertex,
        iter_edges_e: funarg.iter_edges_e
      });
  var md = function (g) {
    var gref = {
      contents: g
    };
    var gtri = g;
    var n = Curry._1(funarg.nb_vertex, g);
    var tri = /* [] */0;
    var ord = /* [] */0;
    var i = 0;
    while(!Curry._1(CT.is_chordal, gtri) && i < n) {
      var x = Curry._3(funarg.fold_vertex, (function (v$p, x) {
              var deg$p = Curry._2(funarg.out_degree, gref.contents, v$p);
              if (x !== undefined && deg$p > x[1]) {
                return x;
              } else {
                return [
                        v$p,
                        deg$p
                      ];
              }
            }), gref.contents, undefined);
      var v = x !== undefined ? x[0] : Pervasives.failwith("Expecting some vertex");
      var ng = Curry._2(funarg.succ, gref.contents, v);
      var match = List.fold_left((function(ng){
          return function (param, v) {
            var g = param[0];
            var tri$p = List.fold_left((function (tri, v$p) {
                    if (Caml_obj.caml_notequal(v, v$p) && !Curry._3(funarg.mem_edge, g, v, v$p)) {
                      return {
                              hd: [
                                v,
                                v$p
                              ],
                              tl: tri
                            };
                    } else {
                      return tri;
                    }
                  }), param[1], ng);
            var g$p = List.fold_left((function (g, v$p) {
                    if (Caml_obj.caml_notequal(v, v$p)) {
                      return Curry._3(funarg.add_edge, g, v, v$p);
                    } else {
                      return g;
                    }
                  }), g, ng);
            return [
                    g$p,
                    tri$p
                  ];
          }
          }(ng)), [
            gref.contents,
            /* [] */0
          ], ng);
      var tri$p = match[1];
      ord = {
        hd: v,
        tl: ord
      };
      gtri = List.fold_left((function (g, param) {
              return Curry._3(funarg.add_edge, g, param[0], param[1]);
            }), gtri, tri$p);
      gref.contents = Curry._2(funarg.remove_vertex, match[0], v);
      tri = Pervasives.$at(tri$p, tri);
      i = i + 1 | 0;
    };
    return [
            gtri,
            tri,
            ord
          ];
  };
  var triangulate = function (g) {
    return md(g)[0];
  };
  return {
          md: md,
          triangulate: triangulate
        };
}

function I(funarg) {
  $$Set.Make(funarg.V);
  var CT = Cliquetree.CliqueTree(funarg);
  Oper.Choose({
        iter_vertex: funarg.iter_vertex,
        iter_edges_e: funarg.iter_edges_e
      });
  var $$let = funarg.V;
  var partial_arg_V = {
    hash: $$let.hash,
    equal: $$let.equal
  };
  var partial_arg_fold_vertex = funarg.fold_vertex;
  var partial_arg = {
    V: partial_arg_V,
    fold_vertex: partial_arg_fold_vertex
  };
  var include = Builder.I(funarg);
  var Copy = Gmap.Vertex(partial_arg, {
        empty: include.empty,
        add_vertex: include.add_vertex
      });
  var md = function (g) {
    var gtri = Curry._2(Copy.map, (function (x) {
            return x;
          }), g);
    var gcur = Curry._2(Copy.map, (function (x) {
            return x;
          }), g);
    var n = Curry._1(funarg.nb_vertex, g);
    var tri = /* [] */0;
    var ord = /* [] */0;
    var i = 0;
    while(!Curry._1(CT.is_chordal, gtri) && i < n) {
      var x = Curry._3(funarg.fold_vertex, (function (v$p, x) {
              var deg$p = Curry._2(funarg.out_degree, gcur, v$p);
              if (x !== undefined && deg$p > x[1]) {
                return x;
              } else {
                return [
                        v$p,
                        deg$p
                      ];
              }
            }), gcur, undefined);
      var v = x !== undefined ? x[0] : Pervasives.failwith("Expecting some vertex");
      var ng = Curry._2(funarg.succ, gcur, v);
      var tri$p = List.fold_left((function(ng){
          return function (tri, v) {
            return List.fold_left((function (tri, v$p) {
                          var tri$p = Caml_obj.caml_notequal(v, v$p) && !Curry._3(funarg.mem_edge, g, v, v$p) ? ({
                                hd: [
                                  v,
                                  v$p
                                ],
                                tl: tri
                              }) : tri;
                          List.iter((function (v$p) {
                                  if (Caml_obj.caml_notequal(v, v$p)) {
                                    return Curry._3(funarg.add_edge, gcur, v, v$p);
                                  }
                                  
                                }), ng);
                          return tri$p;
                        }), tri, ng);
          }
          }(ng)), /* [] */0, ng);
      ord = {
        hd: v,
        tl: ord
      };
      List.iter((function (param) {
              return Curry._3(funarg.add_edge, gtri, param[0], param[1]);
            }), tri$p);
      Curry._2(funarg.remove_vertex, gcur, v);
      tri = Pervasives.$at(tri$p, tri);
      i = i + 1 | 0;
    };
    return [
            gtri,
            tri,
            ord
          ];
  };
  var triangulate = function (g) {
    return md(g)[0];
  };
  return {
          md: md,
          triangulate: triangulate
        };
}

export {
  P ,
  I ,
  
}
/* Cliquetree Not a pure module */
