// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Utils from "../Utils.mjs";
import * as $$String from "rescript/lib/es6/string.js";
import * as Belt_List from "rescript/lib/es6/belt_List.js";
import * as Belt_MapString from "rescript/lib/es6/belt_MapString.js";

function log(prim) {
  console.log(prim);
}

function logList(l) {
  var prim = Belt_List.toArray(l);
  console.log(prim);
}

function log2(x, y) {
  console.log(y, x);
}

function logList2(l, str) {
  var x = Belt_List.toArray(l);
  console.log(str, x);
}

function mapListToString(m) {
  var b = Belt_MapString.toList(m);
  return $$String.concat(", ", Belt_List.map(b, (function (param) {
                    return "" + param[0] + ": " + param[1] + "";
                  })));
}

function logStrMapList(m, str) {
  var prim = "" + str + ": " + mapListToString(m) + "";
  console.log(prim);
}

function string_of_state(param) {
  var d_str = Curry._1(Utils.Printable.MapString.Int.toString, param.d);
  var f_str = Curry._1(Utils.Printable.MapString.Int.toString, param.f);
  var pred_str = Curry._1(Utils.Printable.MapString.$$String.toString, param.pred);
  return " d = " + d_str + "\n f = " + f_str + "\n pred = " + pred_str + "\n";
}

function depth_first_search(g) {
  var node = function (param, u) {
    var match = param[1];
    var color = match.color;
    var pred = match.pred;
    var f = match.f;
    var d = match.d;
    var t = param[0];
    var dfs_visit = function (t, u, param) {
      var edge = function (param, v) {
        var match = param[1];
        var color = match.color;
        var pred = match.pred;
        var f = match.f;
        var d = match.d;
        var t = param[0];
        if (Belt_MapString.getExn(color, v) === /* White */0) {
          return dfs_visit(t, v, {
                      d: d,
                      f: f,
                      pred: Belt_MapString.set(pred, v, u),
                      color: color
                    });
        } else {
          return [
                  t,
                  {
                    d: d,
                    f: f,
                    pred: pred,
                    color: color
                  }
                ];
        }
      };
      var t$1 = t + 1 | 0;
      var match = Belt_List.reduce(Belt_MapString.getExn(g, u), [
            t$1,
            {
              d: Belt_MapString.set(param.d, u, t$1),
              f: param.f,
              pred: param.pred,
              color: Belt_MapString.set(param.color, u, /* Gray */1)
            }
          ], edge);
      var match$1 = match[1];
      var t$2 = match[0] + 1 | 0;
      return [
              t$2,
              {
                d: match$1.d,
                f: Belt_MapString.set(match$1.f, u, t$2),
                pred: match$1.pred,
                color: Belt_MapString.set(match$1.color, u, /* Black */2)
              }
            ];
    };
    if (Belt_MapString.getExn(color, u) === /* White */0) {
      return dfs_visit(t, u, {
                  d: d,
                  f: f,
                  pred: pred,
                  color: color
                });
    } else {
      return [
              t,
              {
                d: d,
                f: f,
                pred: pred,
                color: color
              }
            ];
    }
  };
  var v = Belt_List.reduce(Belt_MapString.toList(g), /* [] */0, (function (acc, param) {
          return {
                  hd: param[0],
                  tl: acc
                };
        }));
  var initial_state_color = Belt_List.reduceReverse(v, undefined, (function (m, x) {
          return Belt_MapString.set(m, x, /* White */0);
        }));
  var initial_state = {
    d: undefined,
    f: undefined,
    pred: undefined,
    color: initial_state_color
  };
  return Belt_List.reduceReverse(v, [
                0,
                initial_state
              ], node)[1];
}

var Dfs = {
  string_of_state: string_of_state,
  depth_first_search: depth_first_search
};

var g = Belt_List.reduceReverse({
      hd: [
        "u",
        {
          hd: "v",
          tl: {
            hd: "x",
            tl: /* [] */0
          }
        }
      ],
      tl: {
        hd: [
          "v",
          {
            hd: "y",
            tl: /* [] */0
          }
        ],
        tl: {
          hd: [
            "w",
            {
              hd: "z",
              tl: {
                hd: "y",
                tl: /* [] */0
              }
            }
          ],
          tl: {
            hd: [
              "x",
              {
                hd: "v",
                tl: /* [] */0
              }
            ],
            tl: {
              hd: [
                "y",
                {
                  hd: "x",
                  tl: /* [] */0
                }
              ],
              tl: {
                hd: [
                  "z",
                  {
                    hd: "z",
                    tl: /* [] */0
                  }
                ],
                tl: /* [] */0
              }
            }
          }
        }
      }
    }, undefined, (function (m, param) {
        return Belt_MapString.set(m, param[0], param[1]);
      }));

var s = depth_first_search(g);

var prim = string_of_state(s);

console.log(prim);

var Str_map;

export {
  Str_map ,
  log ,
  logList ,
  log2 ,
  logList2 ,
  mapListToString ,
  logStrMapList ,
  Dfs ,
}
/* g Not a pure module */
