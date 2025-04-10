// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Jest from "@glennsl/rescript-jest/src/jest.res.mjs";
import * as Monad_Reader from "../src/Monads/Monad_Reader.res.mjs";

Jest.test("Reader Type", () => Jest.Expect.toEqual(Jest.Expect.expect(1), 1));

Jest.test("run()", () => {
  let result = Monad_Reader.run({
    TAG: "Reader",
    _0: e => e + 1 | 0
  }, 1);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 2);
});

Jest.test("return()", () => {
  let result = Monad_Reader.run(Monad_Reader.$$return(99), 1);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 99);
});

Jest.test("ask()", () => {
  let result = Monad_Reader.run(Monad_Reader.ask(), 123);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 123);
});

Jest.test("local()", () => {
  let __x = {
    TAG: "Reader",
    _0: e => e + 1 | 0
  };
  let result = Monad_Reader.run(Monad_Reader.local(e => -e | 0, __x), 1);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 0);
});

Jest.test("map()", () => {
  let __x = {
    TAG: "Reader",
    _0: e => e + 1 | 0
  };
  let result = Monad_Reader.run(Monad_Reader.map(x => x * 10 | 0, __x), 1);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 20);
});

Jest.test("bind() 1", () => {
  let __x = {
    TAG: "Reader",
    _0: e => e + 1 | 0
  };
  let result = Monad_Reader.run(Monad_Reader.bind(x => ({
    TAG: "Reader",
    _0: param => (x << 1)
  }), __x), 1);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 4);
});

Jest.test("bind() 2", () => {
  let __x = {
    TAG: "Reader",
    _0: e => e + 1 | 0
  };
  let result = Monad_Reader.run(Monad_Reader.bind(x => Monad_Reader.$$return((x << 1)), __x), 1);
  return Jest.Expect.toEqual(Jest.Expect.expect(result), 4);
});

Jest.test("bind() 3", () => {
  let greet = (name, greeting) => greeting + ": " + name;
  let ra = {
    TAG: "Reader",
    _0: __x => greet("One", __x)
  };
  let rb = {
    TAG: "Reader",
    _0: __x => greet("Two", __x)
  };
  let rc = {
    TAG: "Reader",
    _0: __x => greet("Three", __x)
  };
  let r12 = Monad_Reader.bind(a => Monad_Reader.bind(b => Monad_Reader.bind(c => Monad_Reader.$$return([
    a,
    b,
    c
  ]), rc), rb), ra);
  let result = Monad_Reader.run(r12, "Hello");
  return Jest.Expect.toEqual(Jest.Expect.expect(result), [
    "Hello: One",
    "Hello: Two",
    "Hello: Three"
  ]);
});

Jest.test("bind() 3.1", () => {
  let greet = (name, greeting) => greeting + ": " + name;
  let __x = {
    TAG: "Reader",
    _0: extra => greet("One", extra)
  };
  let result = Monad_Reader.run(Monad_Reader.bind(a => {
    let __x = {
      TAG: "Reader",
      _0: extra => greet("Two", extra)
    };
    return Monad_Reader.bind(b => {
      let __x = {
        TAG: "Reader",
        _0: extra => greet("Three", extra)
      };
      return Monad_Reader.bind(c => Monad_Reader.$$return([
        a,
        b,
        c
      ]), __x);
    }, __x);
  }, __x), "Hey");
  return Jest.Expect.toEqual(Jest.Expect.expect(result), [
    "Hey: One",
    "Hey: Two",
    "Hey: Three"
  ]);
});

/*  Not a pure module */
