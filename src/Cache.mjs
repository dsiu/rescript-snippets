// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Js_dict from "rescript/lib/es6/js_dict.js";

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

function connect$1() {
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

connect$1();

var cache$1 = {};

function connect$2() {
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

export {
  Database ,
  cache$1 as cache,
  connect$2 as connect,
}
/*  Not a pure module */
