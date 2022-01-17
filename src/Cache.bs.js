// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_dict = require("rescript/lib/js/js_dict.js");

function connect(id) {
  return {
          id: id
        };
}

var Database = {
  connect: connect
};

var cache = {
  contents: undefined
};

function connect$1(param) {
  var client = cache.contents;
  if (client !== undefined) {
    return client;
  }
  var client$1 = {
    id: "abc"
  };
  cache.contents = client$1;
  return client$1;
}

connect$1(undefined);

var cache$1 = {};

function connect$2(param) {
  var key = "client";
  var optClient = Js_dict.get(cache$1, key);
  if (optClient !== undefined) {
    return optClient;
  }
  var client = {
    id: "abc"
  };
  cache$1[key] = client;
  return client;
}

exports.Database = Database;
exports.cache = cache$1;
exports.connect = connect$2;
/*  Not a pure module */
