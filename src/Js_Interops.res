//
// use native js comparison operators
//
// ref:
// https://forum.rescript-lang.org/t/comparison-operators/2662/2

type t
let isBefore: (t, t) => bool = %raw(`(a, b) => a < b`)
