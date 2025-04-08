// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Fs from "fs";
import * as Js_promise from "rescript/lib/es6/Js_promise.js";
import * as Belt_Option from "rescript/lib/es6/Belt_Option.js";

function callbackWithResult(f) {
  return (error, result) => {
    if (!(error == null)) {
      return f({
        TAG: "Error",
        _0: error
      });
    }
    if (!(result == null)) {
      return f({
        TAG: "Ok",
        _0: result
      });
    }
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "nodeCallback arguments invalid",
      Error: new Error()
    };
  };
}

function onResult(result) {
  let message;
  message = result.TAG === "Ok" ? "Success: " + result._0 : "Error: " + Belt_Option.getWithDefault(result._0.message, "Unknown");
  console.log(message);
}

Fs.readFile("hello.txt", "UTF-8", callbackWithResult(onResult));

function callbackWithSuccessOrError(onSuccess, onError) {
  return (error, result) => {
    if (!(error == null)) {
      return onError(error);
    }
    if (!(result == null)) {
      return onSuccess(result);
    }
    throw {
      RE_EXN_ID: "Invalid_argument",
      _1: "nodeCallback arguments invalid",
      Error: new Error()
    };
  };
}

function onSuccess(result) {
  let message = "Success: " + result;
  console.log(message);
}

function onError(error) {
  let message = "Error: " + Belt_Option.getWithDefault(error.message, "Unknown");
  console.log(message);
}

Fs.readFile("hello.txt", "UTF-8", callbackWithSuccessOrError(onSuccess, onError));

function callbackWithPromise(f) {
  return (error, result) => {
    if (error == null) {
      if (result == null) {
        return f(Promise.reject({
          RE_EXN_ID: "Failure",
          _1: "nodeCallback arguments invalid"
        }));
      } else {
        return f(Promise.resolve(result));
      }
    }
    let message = Belt_Option.getWithDefault(error.message, "Unknown");
    f(Promise.reject({
      RE_EXN_ID: "Failure",
      _1: message
    }));
  };
}

function handlePromise(promise) {
  let __x = Js_promise.then_(result => Promise.resolve("Success: " + result), promise);
  let __x$1 = Js_promise.$$catch(_error => Promise.resolve("Error: Unknown"), __x);
  Js_promise.then_(message => {
    console.log(message);
    return Promise.resolve();
  }, __x$1);
}

Fs.readFile("hello.txt", "UTF-8", callbackWithPromise(handlePromise));

export {
  callbackWithResult,
  onResult,
  callbackWithSuccessOrError,
  onSuccess,
  onError,
  callbackWithPromise,
  handlePromise,
}
/*  Not a pure module */
