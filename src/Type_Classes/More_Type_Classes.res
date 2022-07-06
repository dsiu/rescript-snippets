//
// More type classes
// https://blog.shaynefletcher.org/2017/05/more-type-classes-in-ocaml.html
//
let log = Js.log
let log2 = Js.log2

// compose

// ( << )
let compose = (f, g, x) => f(g(x))

// ( >> )
let compose_right = (f, g) => compose(g, f)

let id = x => x

let const = (x, _) => x

//
// Functors
//
// The Functor type class captures the pattern of mapping over the value(s) of some parameterized data type.
module type FUNCTOR = {
  type t<'a>
  let map: ('a => 'b, t<'a>) => t<'b>
}
// In order for a type to qualify as a functor, one need to provide an implementation for map
// (fmap in Haskell) that satisfies the signature.

// Here, map is the standard map function over lists. In OCaml we create a module implementing the
// FUNCTOR signature, which for lists may look like:
module ListFunctor: FUNCTOR with type t<'a> = list<'a> = {
  type t<'a> = list<'a>
  let map = f => List.map(f)
}

/*
 One difference is that the module is named which allows for multiple instances of the same
 signature for the same type to coexist. The with type construct is required in order to be able
 to export the type 'a t specified by the signature. It makes the fact that ListFunctor.t is
 indeed the type list transparent, allowing us to apply ListFunctor.map to ordinary lists.
 */

/*
  An immediate advantage of capturing the functor pattern explicitly via a signature (FUNCTOR) is
  that it enables us to to define an additional parameterized module with tests for validating any
  concrete implementation of the signature:
 */
module TestFunctor = (F: FUNCTOR) => {
  let test_id = x => F.map(id, x) == x

  let test_compose = xs => {
    let f = x => mod(x, 2)
    let g = x => x - 1
    F.map(compose(f, g), xs) == F.map(f, F.map(g, xs))
  }
}

/*
  The tests here correspond to the two functor laws stated above.
  For instance to test ListFunctor we first apply TestFunctor to this module in order to
  retrieve a specialized version:
 */

module TFL = TestFunctor(ListFunctor)

"Test List Functor"->log
TFL.test_id(list{})->log
TFL.test_id(list{1, 2})->log
TFL.test_compose(list{})->log
TFL.test_compose(list{1, 2, 3})->log

/*
  The option type in OCaml also forms a functor:
*/
module OptionFunctor: FUNCTOR with type t<'a> = option<'a> = {
  type t<'a> = option<'a>

  let map = (f, x) =>
    switch x {
    | Some(x) => Some(f(x))
    | None => None
    }
}

// And similar to the list example, we get a test module for free:
module TOF = TestFunctor(OptionFunctor)

"Test Option Functor"->log
TOF.test_id(Some(42))->log
TOF.test_id(None)->log
TOF.test_compose(Some(42))->log
TOF.test_compose(None)->log

//
// Monoids
//

/*
 Any type qualifying as a monoid must have identity value (mempty) and a binary operator (mappend)
 for composing any two elements.
 The OCaml version can be specified by the following module type:
*/

module type MONOID = {
  type t
  let empty: t
  let append: (t, t) => t
}

// There are also a few laws that instances should obey:
module TestMonoid = (M: MONOID) => {
  let test_left_id = x => M.append(M.empty, x) == x
  let test_right_id = x => M.append(x, M.empty) == x

  let test_assoc = (x, y, z) => {
    M.append(x, M.append(y, z)) == M.append(M.append(x, y), z)
  }
}

// One of the more famous monoids is given by the natural numbers with addition and identity element 0:
module IntAddMonoid: MONOID with type t = int = {
  type t = int
  let empty = 0
  let append = \"+"
}

/*
  Another advantage of formalizing patterns by explicit signatures is that it enables us to define
  derived combinators generically. For example, the append operation from IntAddMonoid can be
  lifted to a sum function accepting a list of integers, adding them together or defaulting to 0
  if the empty list is given:
*/
{
  open IntAddMonoid
  let sum = xs => List.fold_left(append, empty, xs)
  sum(list{1, 10, 102})->log
}

module MonoidUtils = (M: MONOID) => {
  include M
  let \"<+>" = (x, y) => append(x, y) // infix version of append
  let concat = xs => List.fold_left(\"<+>", empty, xs)
}

// Pseudo-code - not valid
/*
module ListMonoid: MONOID with type t = list<'a> = {
  type t = list<'a>
  let empty = list{}
  let append = (x, y) => List.append(x, y)
}
*/

/*
  However it is not possible to directly parameterize modules by types. A work around can be
  achieved by first introducing a dummy module for wrapping the type and passing it along as a
  module parameter:
*/
module type TYPE = {
  type t
}

module ListMonoid = (T: TYPE): (MONOID with type t = list<T.t>) => {
  type t = list<T.t>
  let empty = list{}
  let append = (xs, ys) => \"@"(xs, ys)
}
