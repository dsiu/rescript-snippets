// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Map = require("rescript/lib/js/map.js");
var $$Set = require("rescript/lib/js/set.js");
var List = require("rescript/lib/js/list.js");
var Curry = require("rescript/lib/js/curry.js");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Caml_js_exceptions = require("rescript/lib/js/caml_js_exceptions.js");

function Make(funarg, funarg$1) {
  var M = $$Map.Make(funarg.V);
  var N = $$Set.Make(funarg.V);
  var analyze = function (initial, g) {
    var match = Curry._3(funarg.fold_vertex, (function (vertex, param) {
            return [
                    Curry._2(N.add, vertex, param[0]),
                    Curry._3(M.add, vertex, Curry._1(initial, vertex), param[1])
                  ];
          }), g, [
          N.empty,
          M.empty
        ]);
    var add = funarg$1.direction ? (function (n) {
          var succs = Curry._2(funarg.succ_e, g, n);
          return List.map((function (edge) {
                        return [
                                Curry._1(funarg$1.analyze, edge),
                                Curry._1(funarg.E.dst, edge)
                              ];
                      }), succs);
        }) : (function (n) {
          var preds = Curry._2(funarg.pred_e, g, n);
          return List.map((function (edge) {
                        return [
                                Curry._1(funarg$1.analyze, edge),
                                Curry._1(funarg.E.src, edge)
                              ];
                      }), preds);
        });
    var nodemap = Curry._3(funarg.fold_vertex, (function (vertex, m) {
            return Curry._3(M.add, vertex, Curry._1(add, vertex), m);
          }), g, M.empty);
    var worklist = function (_data, _wl) {
      while(true) {
        var wl = _wl;
        var data = _data;
        var meet = function ($$default, param) {
          if (!param) {
            return $$default;
          }
          var xs = param.tl;
          var x = param.hd;
          if (xs) {
            return List.fold_left((function (a, b) {
                          return Curry._2(funarg$1.join, a, b);
                        }), x, xs);
          } else {
            return x;
          }
        };
        var n;
        try {
          n = Caml_option.some(Curry._1(N.choose, wl));
        }
        catch (raw_exn){
          var exn = Caml_js_exceptions.internalToOCamlException(raw_exn);
          if (exn.RE_EXN_ID === "Not_found") {
            n = undefined;
          } else {
            throw exn;
          }
        }
        if (n === undefined) {
          return data;
        }
        var n$1 = Caml_option.valFromOption(n);
        var wl$1 = Curry._2(N.remove, n$1, wl);
        var match;
        if (funarg$1.direction) {
          var new_node_data = function (data, node) {
            var edges = Curry._2(M.find, node, nodemap);
            var analysis = List.map((function (param) {
                    return Curry._1(param[0], Curry._2(M.find, param[1], data));
                  }), edges);
            var node_data = Curry._2(M.find, node, data);
            var node_data$p = meet(node_data, analysis);
            if (Curry._2(funarg$1.equal, node_data, node_data$p)) {
              return ;
            } else {
              return Caml_option.some(Curry._3(M.add, node, node_data$p, data));
            }
          };
          match = [
            new_node_data,
            Curry._2(funarg.pred, g, n$1)
          ];
        } else {
          var new_node_data$1 = function (data, node) {
            var edges = Curry._2(M.find, node, nodemap);
            var analysis = List.map((function (param) {
                    return Curry._1(param[0], Curry._2(M.find, param[1], data));
                  }), edges);
            var node_data = Curry._2(M.find, node, data);
            var node_data$p = meet(node_data, analysis);
            if (Curry._2(funarg$1.equal, node_data, node_data$p)) {
              return ;
            } else {
              return Caml_option.some(Curry._3(M.add, node, node_data$p, data));
            }
          };
          match = [
            new_node_data$1,
            Curry._2(funarg.succ, g, n$1)
          ];
        }
        var f = match[0];
        var match$1 = List.fold_left((function(f){
            return function (param, n) {
              var d = param[0];
              var wl = param[1];
              var d$p = Curry._2(f, d, n);
              if (d$p !== undefined) {
                return [
                        Caml_option.valFromOption(d$p),
                        Curry._2(N.add, n, wl)
                      ];
              } else {
                return [
                        d,
                        wl
                      ];
              }
            }
            }(f)), [
              data,
              wl$1
            ], match[1]);
        _wl = match$1[1];
        _data = match$1[0];
        continue ;
      };
    };
    var data = worklist(match[1], match[0]);
    return function (n) {
      return Curry._2(M.find, n, data);
    };
  };
  return {
          analyze: analyze
        };
}

exports.Make = Make;
/* No side effect */
