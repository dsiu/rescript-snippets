// Using first class module in Rescript
//
// ref:https://forum.rescript-lang.org/t/confused-about-using-belt-mutablemap/3234
//

// johnj
// You need to use a first-class module 2 to create a custom Belt.MutableMap.t. IMO, the first-class modules aren’t very well documented yet, but the basic idea is that they turn modules into values. Ordinarily, modules (module M = ...) and values (let x = ...) live on different “layers” of the language, and they can’t interact directly. With first-class modules, you can pass a module to a function like it was a value.
//
// Here’s a working example. The easiest way is generally using Belt.Id.MakeComparable.

module M = {
  type t = int
  let cmp = (m1: t, m2: t) => m1 - m2
}

// Create an new Id module
module M_Id = Belt.Id.MakeComparable(M)

// use module() to turn M_Id into a first-class module
let map = Belt.MutableMap.make(~id=module(M_Id))
Belt.MutableMap.set(map, 1, "a")

//You can use Belt.Id.comparable too, although it’s slightly different. It directly creates a first-class module for you. However, due to some advanced type-system quirks, ReScript requires that you “unpack” the FCM into a normal module anyway before you can use it.

module M1 = {
  type t = int
}

// Create an new Id module
module M1_Id = unpack(Belt.Id.comparable(~cmp=(m1: M1.t, m2: M1.t) => m1 - m2))

// use module() to turn M_Id into a first-class module
let map = Belt.MutableMap.make(~id=module(M1_Id))
Belt.MutableMap.set(map, 1, "a")

//
// Document First Class Modules #155
// https://github.com/rescript-association/rescript-lang.org/issues/155
//
// here's pretty much the whole syntax surface in one snippet:

module type X = {
  type t
  let add: (int, int) => int
}

// convert module to first-class module
type f = module(X)

// create a first-class module
let a = module(
  {
    type t
    let add = (a, b) => a + b
  }: X
)

// convert first-class module to module (in a module scope only)
module B = unpack(a)
