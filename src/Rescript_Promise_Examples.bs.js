// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Js_exn = require("rescript/lib/js/js_exn.js");
var $$Promise = require("@ryyppy/rescript-promise/src/Promise.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

var p1 = new Promise((function (resolve, _reject) {
        return resolve("hello world");
      }));

console.log(p1);

var p2 = Promise.resolve("some value");

console.log(p2);

var MyOwnError = /* @__PURE__ */Caml_exceptions.create("Rescript_Promise_Examples.MyOwnError");

var p3 = Promise.reject({
      RE_EXN_ID: MyOwnError,
      _1: "some rejection"
    });

console.log(p3);

Promise.resolve("hello world").then(function (msg) {
        return Promise.resolve("Message: " + msg);
      }).then(function (msg) {
      console.log(msg);
      return Promise.resolve(undefined);
    });

function queryComments(username) {
  var tmp = username === "patrick" ? [
      "comment 1",
      "comment 2"
    ] : [];
  return Promise.resolve(tmp);
}

function queryUser(param) {
  return Promise.resolve({
              name: "patrick"
            });
}

queryUser("u1").then(function (user) {
        return queryComments(user.name);
      }).then(function (comments) {
      Belt_Array.forEach(comments, (function (comment) {
              console.log(comment);
              
            }));
      return Promise.resolve(undefined);
    });

function createNumPromise(n) {
  return Promise.resolve(n);
}

Promise.resolve(5).then(function (num) {
        return num + 1 | 0;
      }).then(function (num) {
      console.log(num);
      
    });

var MyError = /* @__PURE__ */Caml_exceptions.create("Rescript_Promise_Examples.MyError");

$$Promise.$$catch(Promise.reject({
              RE_EXN_ID: MyError,
              _1: "test"
            }).then(function (str) {
            console.log("this should not be reached: " + str);
            return Promise.resolve({
                        TAG: /* Ok */0,
                        _0: "successful"
                      });
          }), (function (e) {
          var err = e.RE_EXN_ID === MyError ? "found MyError: " + e._1 : "Some unknown error";
          return Promise.resolve({
                      TAG: /* Error */1,
                      _0: err
                    });
        })).then(function (result) {
      var msg;
      msg = result.TAG === /* Ok */0 ? "Successful: " + result._0 : "Error: " + result._0;
      console.log(msg);
      return Promise.resolve(undefined);
    });

function causeErr(param) {
  return Promise.resolve(Js_exn.raiseError("Some JS error"));
}

$$Promise.$$catch(Promise.resolve(undefined).then(function (param) {
          return Promise.resolve(Js_exn.raiseError("Some JS error"));
        }), (function (e) {
        if (e.RE_EXN_ID === $$Promise.JsError) {
          var msg = e._1.message;
          if (msg !== undefined) {
            console.log("Some JS error msg: " + msg);
          } else {
            console.log("Must be some non-error value");
          }
        } else {
          console.log("Some unknown error");
        }
        return Promise.resolve(undefined);
      }));

var TestError = /* @__PURE__ */Caml_exceptions.create("Rescript_Promise_Examples.TestError");

function causeJsErr(param) {
  return Js_exn.raiseError("Some JS error");
}

function causeReScriptErr(param) {
  throw {
        RE_EXN_ID: TestError,
        _1: "Some ReScript error",
        Error: new Error()
      };
}

$$Promise.$$catch(Promise.resolve(undefined).then(function (param) {
          if (generateRandomInt() > 5) {
            throw {
                  RE_EXN_ID: TestError,
                  _1: "Some ReScript error",
                  Error: new Error()
                };
          }
          return Promise.resolve(Js_exn.raiseError("Some JS error"));
        }), (function (e) {
        if (e.RE_EXN_ID === TestError) {
          console.log("ReScript Error caught:" + e._1);
        } else if (e.RE_EXN_ID === $$Promise.JsError) {
          var msg = e._1.message;
          if (msg !== undefined) {
            console.log("Some JS error msg: " + msg);
          } else {
            console.log("Must be some non-error value");
          }
        } else {
          console.log("Some unknown error");
        }
        return Promise.resolve(undefined);
      }));

someAsyncApi().then(function (str) {
      return Promise.resolve((console.log(str), undefined));
    });

var place = {
  contents: 0
};

function delayedMsg(ms, msg) {
  return new Promise((function (resolve, param) {
                setTimeout((function (param) {
                        place.contents = place.contents + 1 | 0;
                        return resolve([
                                    place.contents,
                                    msg
                                  ]);
                      }), ms);
                
              }));
}

var p1$1 = delayedMsg(1000, "is Anna");

var p2$1 = delayedMsg(500, "myName");

var p3$1 = delayedMsg(100, "Hi");

Promise.all([
        p1$1,
        p2$1,
        p3$1
      ]).then(function (arr) {
      Belt_Array.forEach(arr, (function (param) {
              console.log("Place " + String(param[0]) + " => " + param[1]);
              
            }));
      return Promise.resolve(undefined);
    });

function racer(ms, name) {
  return new Promise((function (resolve, param) {
                setTimeout((function (param) {
                        return resolve(name);
                      }), ms);
                
              }));
}

var promises = [
  racer(1000, "Turtle"),
  racer(500, "Hare"),
  racer(100, "Eagle")
];

Promise.race(promises).then(function (winner) {
      return Promise.resolve((console.log("Congrats: " + winner), undefined));
    });

$$Promise.$$catch(Promise.resolve(1).then(function (value) {
            return Promise.resolve(Promise.resolve(value + 2 | 0));
          }).then(function (p) {
          return p.then(function (n) {
                      return Promise.resolve((console.log(n), undefined));
                    });
        }), (function (e) {
        console.log("luckily, our mistake will be caught here");
        console.log(e);
        return Promise.resolve(undefined);
      }));

$$Promise.$$catch(Promise.resolve(1).then(function (value) {
            return Promise.resolve(value);
          }).then(function (p) {
          p.then(function (n) {
                console.log(n);
                
              });
          
        }), (function (e) {
        console.log("luckily, our mistake will be caught here");
        return Promise.resolve(undefined);
      }));

var queryUser$1;

exports.MyOwnError = MyOwnError;
exports.queryComments = queryComments;
exports.queryUser = queryUser$1;
exports.createNumPromise = createNumPromise;
exports.MyError = MyError;
exports.causeErr = causeErr;
exports.TestError = TestError;
exports.causeJsErr = causeJsErr;
exports.causeReScriptErr = causeReScriptErr;
exports.place = place;
exports.delayedMsg = delayedMsg;
exports.p1 = p1$1;
exports.p2 = p2$1;
exports.p3 = p3$1;
exports.racer = racer;
exports.promises = promises;
/* p1 Not a pure module */