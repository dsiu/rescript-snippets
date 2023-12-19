// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.mjs";
import * as Monad_Reader from "../src/Monads/Monad_Reader.mjs";

Jest.test("Reader Type", (function (param) {
        return Jest.Expect.toEqual(Jest.Expect.expect(1), 1);
      }));

Jest.test("run()", (function (param) {
        var result = Monad_Reader.run({
              TAG: "Reader",
              _0: (function (e) {
                  return e + 1 | 0;
                })
            }, 1);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
      }));

Jest.test("return()", (function (param) {
        var result = Monad_Reader.run(Monad_Reader.$$return(99), 1);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 99);
      }));

Jest.test("ask()", (function (param) {
        var result = Monad_Reader.run(Monad_Reader.ask(), 123);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 123);
      }));

Jest.test("local()", (function (param) {
        var __x = {
          TAG: "Reader",
          _0: (function (e) {
              return e + 1 | 0;
            })
        };
        var result = Monad_Reader.run(Monad_Reader.local((function (e) {
                    return -e | 0;
                  }), __x), 1);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 0);
      }));

Jest.test("map()", (function (param) {
        var __x = {
          TAG: "Reader",
          _0: (function (e) {
              return e + 1 | 0;
            })
        };
        var result = Monad_Reader.run(Monad_Reader.map((function (x) {
                    return Math.imul(x, 10);
                  }), __x), 1);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 20);
      }));

Jest.test("bind() 1", (function (param) {
        var __x = {
          TAG: "Reader",
          _0: (function (e) {
              return e + 1 | 0;
            })
        };
        var result = Monad_Reader.run(Monad_Reader.bind((function (x) {
                    return {
                            TAG: "Reader",
                            _0: (function (param) {
                                return (x << 1);
                              })
                          };
                  }), __x), 1);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 4);
      }));

Jest.test("bind() 2", (function (param) {
        var __x = {
          TAG: "Reader",
          _0: (function (e) {
              return e + 1 | 0;
            })
        };
        var result = Monad_Reader.run(Monad_Reader.bind((function (x) {
                    return Monad_Reader.$$return((x << 1));
                  }), __x), 1);
        return Jest.Expect.toEqual(Jest.Expect.expect(result), 4);
      }));

Jest.test("bind() 3", (function (param) {
        var greet = function (name, greeting) {
          return greeting + ": " + name;
        };
        var ra = {
          TAG: "Reader",
          _0: (function (param) {
              return greet("One", param);
            })
        };
        var rb = {
          TAG: "Reader",
          _0: (function (param) {
              return greet("Two", param);
            })
        };
        var rc = {
          TAG: "Reader",
          _0: (function (param) {
              return greet("Three", param);
            })
        };
        var r12 = Monad_Reader.bind((function (a) {
                return Monad_Reader.bind((function (b) {
                              return Monad_Reader.bind((function (c) {
                                            return Monad_Reader.$$return([
                                                        a,
                                                        b,
                                                        c
                                                      ]);
                                          }), rc);
                            }), rb);
              }), ra);
        var result = Monad_Reader.run(r12, "Hello");
        return Jest.Expect.toEqual(Jest.Expect.expect(result), [
                    "Hello: One",
                    "Hello: Two",
                    "Hello: Three"
                  ]);
      }));

Jest.test("bind() 3.1", (function (param) {
        var greet = function (name, greeting) {
          return greeting + ": " + name;
        };
        var __x = {
          TAG: "Reader",
          _0: (function (param) {
              return greet("One", param);
            })
        };
        var result = Monad_Reader.run(Monad_Reader.bind((function (a) {
                    var __x = {
                      TAG: "Reader",
                      _0: (function (param) {
                          return greet("Two", param);
                        })
                    };
                    return Monad_Reader.bind((function (b) {
                                  var __x = {
                                    TAG: "Reader",
                                    _0: (function (param) {
                                        return greet("Three", param);
                                      })
                                  };
                                  return Monad_Reader.bind((function (c) {
                                                return Monad_Reader.$$return([
                                                            a,
                                                            b,
                                                            c
                                                          ]);
                                              }), __x);
                                }), __x);
                  }), __x), "Hey");
        return Jest.Expect.toEqual(Jest.Expect.expect(result), [
                    "Hey: One",
                    "Hey: Two",
                    "Hey: Three"
                  ]);
      }));

export {
  
}
/*  Not a pure module */
