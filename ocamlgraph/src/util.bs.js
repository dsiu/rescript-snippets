// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("rescript/lib/js/curry.js");
var Hashtbl = require("rescript/lib/js/hashtbl.js");

function OTProduct(X, Y) {
  var compare = function (param, param$1) {
    var cv = Curry._2(X.compare, param[0], param$1[0]);
    if (cv !== 0) {
      return cv;
    } else {
      return Curry._2(Y.compare, param[1], param$1[1]);
    }
  };
  return {
          compare: compare
        };
}

function DataV(L, V) {
  var compare = function (param, param$1) {
    return Curry._2(V.compare, param[1], param$1[1]);
  };
  var hash = function (param) {
    return Curry._1(V.hash, param[1]);
  };
  var equal = function (param, param$1) {
    return Curry._2(V.equal, param[1], param$1[1]);
  };
  var create = function (y, lbl) {
    return [
            {
              contents: y
            },
            lbl
          ];
  };
  var label = function (param) {
    return param[1];
  };
  var data = function (param) {
    return param[0].contents;
  };
  var set_data = function (param) {
    var partial_arg = param[0];
    return function (param) {
      partial_arg.contents = param;
      
    };
  };
  return {
          compare: compare,
          hash: hash,
          equal: equal,
          create: create,
          label: label,
          data: data,
          set_data: set_data
        };
}

function HTProduct(funarg, funarg$1) {
  var equal = function (param, param$1) {
    if (Curry._2(funarg.equal, param[0], param$1[0])) {
      return Curry._2(funarg$1.equal, param[1], param$1[1]);
    } else {
      return false;
    }
  };
  var hash = function (param) {
    return Hashtbl.hash([
                Curry._1(funarg.hash, param[0]),
                Curry._1(funarg$1.hash, param[1])
              ]);
  };
  return {
          hash: hash,
          equal: equal
        };
}

function CMPProduct(funarg, funarg$1) {
  var equal = function (param, param$1) {
    if (Curry._2(funarg.equal, param[0], param$1[0])) {
      return Curry._2(funarg$1.equal, param[1], param$1[1]);
    } else {
      return false;
    }
  };
  var hash = function (param) {
    return Hashtbl.hash([
                Curry._1(funarg.hash, param[0]),
                Curry._1(funarg$1.hash, param[1])
              ]);
  };
  var compare = function (param, param$1) {
    var cv = Curry._2(funarg.compare, param[0], param$1[0]);
    if (cv !== 0) {
      return cv;
    } else {
      return Curry._2(funarg$1.compare, param[1], param$1[1]);
    }
  };
  var include = {
    compare: compare
  };
  return {
          compare: include.compare,
          hash: hash,
          equal: equal
        };
}

exports.OTProduct = OTProduct;
exports.HTProduct = HTProduct;
exports.CMPProduct = CMPProduct;
exports.DataV = DataV;
/* No side effect */
