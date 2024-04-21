// Type equivalence
// https://forum.rescript-lang.org/t/type-equivalence/3332/3
//
@@uncurried
@@uncurried.swap

module type SomeId = {
  type t
  let make: int => t
  let getValue: t => int
  let cmp: (. t, t) => int
}

module SomeId: SomeId = {
  type t = int
  let make = x => x
  let getValue = t => t
  let cmp = Pervasives.compare
}

module SomeIdCmp = Belt.Id.MakeComparable(SomeId)

let empty = Belt.Map.make(~id=module(SomeIdCmp))

let someId = SomeId.make(1)
let map = Belt.Map.set(empty, someId, "hello")
// let map = Belt.Map.set(map, 2, "hello") // this will error out
