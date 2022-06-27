module type S = {
  type t<'a>
  let fold: (t<'a>, ~init: 'acc, ~f: ('acc, 'a) => 'acc) => 'acc
}

module type Extension = {
  type t<'a>
  let iter: (t<'a>, ~f: 'a => unit) => unit
  let length: t<'a> => int
  let count: (t<'a>, ~f: 'a => bool) => int
  let for_all: (t<'a>, ~f: 'a => bool) => bool
  let exists: (t<'a>, ~f: 'a => bool) => bool
}

// For extending a Foldable module

module Extend = (Arg: S): (Extension with type t<'a> := Arg.t<'a>) => {
  open Arg

  let iter = (t, ~f) => fold(t, ~init=(), ~f=((), a) => f(a))

  let length = t => fold(t, ~init=0, ~f=(acc, _) => acc + 1)

  let count = (t, ~f) => fold(t, ~init=0, ~f=(count, x) => {count + (f(x) ? 1 : 0)})

  exception Short_circuit

  let for_all = (c, ~f) => {
    try {
      iter(c, ~f=x => !f(x) ? raise(Short_circuit) : ())
      true
    } catch {
    | Short_circuit => false
    }
  }

  let exists = (c, ~f) => {
    try {
      iter(c, ~f=x => f(x) ? raise(Short_circuit) : ())
      true
    } catch {
    | Short_circuit => false
    }
  }
}
