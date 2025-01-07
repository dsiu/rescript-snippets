//
// https://accu.org/journals/overload/25/142/fletcher_2445/
// https://blog.shaynefletcher.org/2016/10/implementing-type-classes-as-ocaml.html
// https://okmij.org/ftp/Computation/typeclass.html
// https://github.com/hongchangwu/ocaml-type-classes
// Implementing Type-Classes as OCaml Modules
//

// Starting with the basics, consider the class of types whose values can be compared for equality.
// Call this type-class Eq . We represent the class as a module signature.
@@uncurried
@@uncurried.swap

@@warning("-44")

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
  \"@@"(Console.log, Show.show(x))
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

// The existence of Num admits writing a polymorphic function sum that will work for any 'a
// list of values if only 'a can be shown to be in Num .
let sum: (num_impl<'a>, list<'a>) => 'a = (type a, num: num_impl<a>, ls: list<a>) => {
  module Num = unpack(num: NUM with type t = a)
  List.fold_right(Num.\"+", ls, Num.from_int(0))
}

let test_sum = sum(num_int, list{1, 2, 3, 4, 20})
test_sum->Js.log //30

// This next function requires evidence of membership in two classes.
let print_incr: ((show_impl<'a>, num_impl<'a>), 'a) => unit = (
  type a,
  (show: show_impl<a>, num: num_impl<a>),
  x: a,
) => {
  module Num = unpack(num: NUM with type t = a)
  open Num
  print(show, x + from_int(1))
}

// (*An instantiation*)
let print_incr_int = (x: int): unit => print_incr((show_int, num_int), x)
print_incr_int(27) // 28

// If 'a is in Show then we can easily extend Show to include the type 'a list .
// As we saw earlier, this kind of thing can be done with an appropriate functor. (See Listing 2.)
module type LIST_SHOW = (X: SHOW) => (SHOW with type t = list<X.t>)

module List_show: LIST_SHOW = (X: SHOW) => {
  type t = list<X.t>

  let show = xs => {
    let rec go = (first, x) =>
      switch x {
      | list{} => "]"
      | list{h, ...t} =>
        if first {
          ""
        } else {
          ", "
        } ++
        (X.show(h) ++
        go(false, t))
      }
    "[" ++ go(true, xs)
  }
}

// There is also another way: one can write a function to dynamically compute an 'a list show_impl
// from an 'a show_impl (see Listing 3).
let show_list: show_impl<'a> => show_impl<list<'a>> = (type a, show: show_impl<a>) => {
  module Show = unpack(show: SHOW with type t = a)
  module(
    {
      type t = list<a>
      let show: t => string = xs => {
        let rec go = (first, x) =>
          switch x {
          | list{} => "]"
          | list{h, ...t} =>
            if first {
              ""
            } else {
              ", "
            } ++
            (Show.show(h) ++
            go(false, t))
          }
        "[" ++ go(true, xs)
      }
    }: SHOW with type t = list<a>
  )
}

let testls: string = {
  module Show = unpack(show_list(show_int): SHOW with type t = list<int>)
  Show.show(list{1, 2, 3})
}

// The type-class Mul is an aggregation of the type-classes Eq and Num together with a function to
// perform multiplication. (Listing 4.)
module type MUL = {
  include EQ
  include NUM with type t := t

  let mul: (t, t) => t
}

type mul_impl<'a> = module(MUL with type t = 'a)

module type MUL_F = (E: EQ, N: NUM with type t = E.t) => (MUL with type t = E.t)

// A default instance of Mul can be provided given compatible instances of Eq and Num .
// (See Listing 5.)
module Mul_default: MUL_F = (E: EQ, N: NUM with type t = E.t) => {
  include (E: EQ with type t = E.t)
  include (N: NUM with type t := E.t)

  let mul: (t, t) => t = {
    let rec loop = (x, y) =>
      switch () {
      | () if eq(x, from_int(0)) => from_int(0)
      | () if eq(x, from_int(1)) => y
      | () => y + loop(x + from_int(-1), y)
      }
    loop
  }
}

module Mul_bool: MUL with type t = bool = Mul_default(Eq_bool, Num_bool)

// Specific instances can be constructed as needs demand (Listing 6).
module Mul_int: MUL with type t = int = {
  include (Eq_int: EQ with type t = int)
  include (Num_int: NUM with type t := int)
  let mul = Pervasives.\"*"
}

let dot: (mul_impl<'a>, list<'a>, list<'a>) => 'a = (type a, mul: mul_impl<a>, xs, ys) => {
  module M = unpack(mul: MUL with type t = a)
  \"@@"(sum(module(M: NUM with type t = a), _), List.map2(M.mul, xs, ys))
}
let test_dot = dot(module(Mul_int: MUL with type t = int), list{1, 2, 3}, list{4, 5, 6})

// Note that in this definition of dot , coercion of the provided Mul instance to its base Num
// instance is performed.

// Listing 7 provides an example of polymorphic recursion utilizing the dynamic production of
// evidence by way of the show_list function presented earlier.
let rec replicate: (int, 'a) => list<'a> = (n, x) =>
  if n <= 0 {
    list{}
  } else {
    list{x, ...replicate(n - 1, x)}
  }

let rec print_nested: 'a. (show_impl<'a>, int, 'a) => unit = (show_mod, x, a) => {
  (
    switch x {
    | 0 => x => print(show_mod, x)
    | n => x => print_nested(show_list(show_mod), n - 1, replicate(n, x))
    }
  )(a)
}

let test_nested = {
  //  let n = read_int()
  let n = 10
  print_nested(module(Show_int: SHOW with type t = int), n, 5)
}
