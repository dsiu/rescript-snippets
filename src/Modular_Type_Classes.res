//
// https://accu.org/journals/overload/25/142/fletcher_2445/
//
// Implementing Type-Classes as OCaml Modules
//

// Starting with the basics, consider the class of types whose values can be compared for equality.
// Call this type-class Eq . We represent the class as a module signature.
module type EQ = {
  type t
  let eq: (t, t) => bool
}

// Specific instances of Eq are modules that implement this signature. Here are two examples.
module Eq_bool: EQ with type t = bool = {
  type t = bool
  let eq = (a, b) => a === b
}

module Eq_int: EQ with type t = int = {
  type t = int
  let eq = (a, b) => a === b
}

// Given instances of class Eq ( X and Y say,) we realize that products of those instances
// are also in Eq . This idea can be expressed as a functor with the following type.
module type EQ_PROD = (X: EQ, Y: EQ) => (EQ with type t = (X.t, Y.t))

// The implementation of this functor is simply stated as the following.
module Eq_prod: EQ_PROD = (X: EQ, Y: EQ) => {
  type t = (X.t, Y.t)
  let eq = ((x1, y1), (x2, y2)) => X.eq(x1, x2) && Y.eq(y1, y2)
}

// With this functor we can build concrete instances for products. Here‘s one example.
module Eq_bool_int: EQ with type t = (bool, int) = Eq_prod(Eq_bool, Eq_int)

// The class Eq can be used as a building block for the construction of new type classes.
// For example, we might define a new type-class Ord that admits types that are equality
// comparable and whose values can be ordered with a ‘less-than’ relation. We introduce a
// new module type to describe this class.
module type ORD = {
  include EQ
  let lt: (t, t) => bool
}

// Here’s an example instance of this class.
module Ord_int: ORD with type t = int = {
  include Eq_int
  let lt = (a, b) => Pervasives.\"<"(a, b)
}

// As before, given two instances of this class, we observe that products of these instances also
// reside in the class. Accordingly, we have this functor type
module type ORD_PROD = (X: ORD, Y: ORD) => (ORD with type t = (X.t, Y.t))

// with the following implementation.
module Ord_prod: ORD_PROD = (X: ORD, Y: ORD) => {
  include Eq_prod(X, Y)
  let lt = ((x1, y1), (x2, y2)) => X.lt(x1, x2) || (X.eq(x1, x2) && Y.lt(y1, y2))
}

// This is the corresponding instance for pairs of integers.
module Ord_int_int = Ord_prod(Ord_int, Ord_int)

// Here’s a simple usage example.
let test_ord_int_int = {
  let x = (1, 2)
  let y = (1, 4)
  !Ord_int_int.eq(x, y) && Ord_int_int.lt(x, y)
}
test_ord_int_int->Js.log

//
// Using type-classes to implement parametric polymorphism
//

// This section begins with the Show type-class.
module type SHOW = {
  type t
  let show: t => string
}

// In what follows, it is convenient to make an alias for module values of this type.
type show_impl<'a> = module(SHOW with type t = 'a)

// Here are two instances of this class...
module Show_int: SHOW with type t = int = {
  type t = int
  let show = Pervasives.string_of_int
}

module Show_bool: SHOW with type t = bool = {
  type t = bool
  let show = x =>
    switch x {
    | true => "True"
    | false => "False"
    }
}

// ...and here these instances are ‘packed’ as values:
let show_int: show_impl<int> = module(Show_int: SHOW with type t = int)
let show_bool: show_impl<bool> = module(Show_bool: SHOW with type t = bool)

// The existence of the Show class is all that is required to enable the writing of our
// first parametrically polymorphic function.
let print: (show_impl<'a>, 'a) => unit = (type a, show: show_impl<a>, x: a) => {
  module Show = unpack(show: SHOW with type t = a)
  \"@@"(print_endline, Show.show(x))
}
let test_print_1: unit = print(show_bool, true)
let test_print_2: unit = print(show_int, 3)

// The function print can be used with values of any type 'a as long as the caller can produce
// evidence of 'a ’s membership in Show (in the form of a compatible instance).

// The function print can be used with values of any type 'a as long as the caller can produce
// evidence of 'a ’s membership in Show (in the form of a compatible instance).
//
// Listing 1 begins with the definition of a type-class Num (the class of additive numbers)
// together with some example instances.

module type NUM = {
  type t
  let from_int: int => t
  let \"+": (t, t) => t
}

type num_impl<'a> = module(NUM with type t = 'a)

module Num_int: NUM with type t = int = {
  type t = int
  let from_int = x => x
  let \"+" = (x, y) => Pervasives.\"+"(x, y)
}

let num_int = module(Num_int: NUM with type t = int)

module Num_bool: NUM with type t = bool = {
  type t = bool
  let from_int = x => {
    switch x {
    | 0 => false
    | _ => true
    }
  }
  let \"+" = (x, y) => {
    switch (x, y) {
    | (true, _) => true
    | (false, y) => y
    }
  }
}

let num_bool = module(Num_bool: NUM with type t = bool)

// [TBC]
