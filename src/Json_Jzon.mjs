// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jzon from "rescript-jzon/src/Jzon.mjs";
import * as Curry from "rescript/lib/es6/curry.js";

var style = Jzon.object2((function (param) {
        return [
                param.size,
                param.color
              ];
      }), (function (param) {
        return {
                TAG: "Ok",
                _0: {
                  size: param[0],
                  color: param[1]
                }
              };
      }), Jzon.field("size", Jzon.$$float), Jzon.field("color", Jzon.string));

var point = Jzon.object4((function (param) {
        return [
                param.x,
                param.y,
                param.z,
                param.style
              ];
      }), (function (param) {
        return {
                TAG: "Ok",
                _0: {
                  x: param[0],
                  y: param[1],
                  z: param[2],
                  style: param[3]
                }
              };
      }), Jzon.field("x", Jzon.$$float), Jzon.field("y", Jzon.$$float), Jzon.default(Jzon.field("z", Jzon.$$float), 0.0), Jzon.optional(Jzon.field("style", style)));

var Codecs = {
  style: style,
  point: point
};

var myJsonData = Curry._1(Jzon.encode(point), {
      x: 1.0,
      y: 2.0,
      z: 3.0,
      style: {
        size: 4.0,
        color: "#fd0"
      }
    });

var myPoint = Curry._1(Jzon.decode(point), myJsonData);

console.log("Json");

console.log(myJsonData);

console.log("ReScript Type");

console.log(myPoint);

export {
  Codecs ,
  myJsonData ,
  myPoint ,
}
/* style Not a pure module */
