// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "rescript/lib/es6/curry.js";
import * as Fetch from "bs-fetch/src/Fetch.mjs";
import * as Core__JSON from "@rescript/core/src/Core__JSON.mjs";
import * as Js_promise from "rescript/lib/es6/js_promise.js";
import * as Core__Option from "@rescript/core/src/Core__Option.mjs";

require('isomorphic-fetch')
;

var __x = fetch("https://aws.random.cat/meow");

var __x$1 = Js_promise.then_(Fetch.$$Response.json, __x);

Js_promise.then_((function (json) {
        return Promise.resolve((console.log(json), undefined));
      }), __x$1);

function catData(data) {
  return {
          file: Core__Option.getExn(Core__Option.flatMap(Core__Option.flatMap(Core__JSON.Decode.object(data), (function (__x) {
                          return __x["file"];
                        })), Core__JSON.Decode.string))
        };
}

var Decode = {
  catData: catData
};

function fetchCat(param) {
  var __x = fetch("https://aws.random.cat/meow");
  var __x$1 = Js_promise.then_(Fetch.$$Response.json, __x);
  return Js_promise.then_((function (obj) {
                return Promise.resolve(catData(obj));
              }), __x$1);
}

var __x$2 = fetchCat();

Js_promise.then_((function (data) {
        return Promise.resolve((console.log(data.file), undefined));
      }), __x$2);

function fetchJson(url, decoder) {
  var __x = fetch(url);
  var __x$1 = Js_promise.then_(Fetch.$$Response.json, __x);
  return Js_promise.then_((function (obj) {
                return Promise.resolve(Curry._1(decoder, obj));
              }), __x$1);
}

function fetchCat$1(param) {
  return fetchJson("https://aws.random.cat/meow", catData);
}

var __x$3 = fetchJson("https://aws.random.cat/meow", catData);

Js_promise.then_((function (data) {
        return Promise.resolve((console.log(data.file), undefined));
      }), __x$3);

export {
  Decode ,
  fetchJson ,
  fetchCat$1 as fetchCat,
}
/*  Not a pure module */
