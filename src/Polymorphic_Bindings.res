//
// polymorphic bindings
//
// https://forum.rescript-lang.org/t/problem-with-polymorphic-bindings/4269/4
//
module type T = {
  type t
}

module Atom = {
  type t<'a>

  module MakeAtomExternals = (T: T) => {
    module Sync = {
      external atom: T.t => t<T.t> = "atom"
      external use: t<T.t> => T.t = "useAtom"
      external get: t<T.t> => T.t = "get"

      external atomThunk: (unit => T.t) => t<unit => T.t> = "atom"
      external useThunk: t<unit => T.t> => T.t = "useAtom"
      external getThunk: t<unit => T.t> => T.t = "get"
    }

    module Async = {
      external atom: promise<T.t> => t<promise<T.t>> = "atom"
      external use: t<promise<T.t>> => T.t = "useAtom"
      external get: t<promise<T.t>> => promise<T.t> = "get"

      external atomThunk: (unit => promise<T.t>) => t<unit => promise<T.t>> = "atom"
      external useThunk: t<unit => promise<T.t>> => T.t = "useAtom"
      external getThunk: t<unit => promise<T.t>> => promise<T.t> = "get"
    }
  }

  // Include as many basic types as you want.
  module Int = MakeAtomExternals({
    type t = int
  })

  module String = MakeAtomExternals({
    type t = string
  })

  module Float = MakeAtomExternals({
    type t = string
  })

  module IntArray = MakeAtomExternals({
    type t = array<int>
  })
}

// These work fine.

let x = [1, 2, 3]->Atom.IntArray.Sync.atom->Atom.IntArray.Sync.use
let y = [1, 2, 3]->Js.Promise.resolve->Atom.IntArray.Async.atom->Atom.IntArray.Async.use

let x' = Atom.String.Async.atomThunk(async () => "yo")->Atom.String.Async.useThunk
let y' = Atom.String.Sync.atomThunk(() => "yo")->Atom.String.Sync.useThunk

// This won't work
// let oops = Atom.String.Sync.atom("yo")->Atom.String.Async.use

// And finally the dreaded sending async stuff to sync....fails to compile!
// let oops = Atom.String.Sync.atomThunk(async () => "yo")->Atom.String.Sync.useThunk

// Library consumer can use the functors on their custom types, too.
module Person = {
  module T = {
    type t = {name: string, age: int}
  }

  include T

  module Atom = {
    include Atom.MakeAtomExternals(T)
  }
}

let person =
  {Person.name: "Ryan", age: 35}->Js.Promise.resolve->Person.Atom.Async.atom->Person.Atom.Async.use

// won't work either
//let oops = Person.Atom.Sync.atomThunk(async () => {Person.name: "Ryan", age: 35})
