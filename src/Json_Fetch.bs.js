// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var $$Promise = require("@ryyppy/rescript-promise/src/Promise.bs.js");

const fetch = require('node-fetch');
if (!globalThis.fetch) {
	globalThis.fetch = fetch;
}
;

var $$Response = {};

var Fetch = {
  $$Response: $$Response
};

function fetchCurrentComic(param) {
  return fetch("http://xkcd.com/info.0.json").then(function (prim) {
              return prim.json();
            });
}

var Xkcd = {
  fetchCurrentComic: fetchCurrentComic
};

$$Promise.$$catch(fetchCurrentComic(undefined).then(function (result) {
          console.log(result);
          
        }), (function (error) {
        console.log(error);
        return Promise.resolve(undefined);
      }));

exports.Fetch = Fetch;
exports.Xkcd = Xkcd;
/*  Not a pure module */