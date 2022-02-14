//
// use native js comparison operators
//
// ref:
// https://forum.rescript-lang.org/t/comparison-operators/2662/2
// https://github.com/yawaramin/bucklescript-bindings-cookbook/blob/master/ReScript.md
//

type t
let isBefore: (t, t) => bool = %raw(`(a, b) => a < b`)

// ref:
// https://kevanstannard.github.io/rescript-blog/global-functions.html
@val external setTimeout: (unit => unit, int) => float = "setTimeout"

let _ = setTimeout(() => {
  Js.log("Hello")
}, 2000)

// How to determine if a global value exists in ReScript?
// ref:
// https://kevanstannard.github.io/rescript-blog/global-values.html
let devOpt = %external(__DEV__)
switch devOpt {
| Some(_) => Js.log("development mode")
| None => Js.log("production mode")
}

// ref:
// https://kevanstannard.github.io/rescript-blog/global-module-functions-and-values.html

// Simple global module value
@val @scope("Math") external random: unit => float = "random"
let someNumber = random()

// Nested global module value
@val @scope(("window", "location", "ancestorOrigins"))
external length: int = "length"

Js.log(length)

// How to define an argument for an external function that has a hard coded value?
// ref:
// https://kevanstannard.github.io/rescript-blog/external-argument-with-hard-coded-value.html

type t1
@send external hi: (t1, @as(json`true`) _, int) => unit = "hi"

let f = x => x->hi(2)

// Which produces:
//function f(x) {
//  x.hi(true, 2);
//}
