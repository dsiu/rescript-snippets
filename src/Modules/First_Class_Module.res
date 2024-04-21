// https://github.com/rescript-association/rescript-lang.org/issues/155
// https://github.com/cca-io/rescript-logger

module type X = {
  type t

  let add: (int, int) => int
}

type f = module(X)

let a = module(
  {
    type t
    let add = (a, b) => a + b
  }: X
)

module B = unpack(a)

B.add(2, 3)->Js.log

//
// https://forum.rescript-lang.org/t/syntax-for-first-class-modules/980/3
//
module type A = {
  type t = int
  let value: t
}

module A: A = {
  type t = int
  let value: t = 42
}

let packedA = module(A: A)

/*
  equivalent to:
  `module UnpackedA: A = (val packedA)`
*/
module UnpackedA: A = unpack(packedA)
