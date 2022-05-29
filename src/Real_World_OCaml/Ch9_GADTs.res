//
// Real World OCaml - Ch.9 GADTs
// https://dev.realworldocaml.org/gadts.html
//

// ordinary variant

type value =
  | Int(int)
  | Bool(bool)

type rec expr =
  | Value(value)
  | Eq(expr, expr)
  | Plus(expr, expr)
  | If(expr, expr, expr)

exception Ill_typed

let rec eval = expr => {
  switch expr {
  | Value(v) => v
  | If(c, t, e) =>
    switch eval(c) {
    | Bool(b) => b ? eval(t) : eval(e)
    | Int(_) => raise(Ill_typed)
    }
  | Eq(x, y) =>
    switch (eval(x), eval(y)) {
    | (Bool(_), _)
    | (_, Bool(_)) =>
      raise(Ill_typed)
    | (Int(f1), Int(f2)) => Bool(f1 == f2)
    }
  | Plus(x, y) =>
    switch (eval(x), eval(y)) {
    | (Bool(_), _)
    | (_, Bool(_)) =>
      raise(Ill_typed)
    | (Int(f1), Int(f2)) => Int(f1 + f2)
    }
  }
}

{
  let i = x => Value(Int(x))
  let b = x => Value(Bool(x))
  let \"+" = (x, y) => Plus(x, y)
  eval(i(3) + i(2))->Js.log
  eval(If(b(true), i(4), i(10)))->Js.log
  // eval(i(3) + b(false))->Js.log // <-- this compiles but throws Ill_typed.  NOT type safe
}

//
// type safe api
//
module type Typesafe_lang_sig = {
  type t<'a>

  // functions for constructing expressions

  let int: int => t<int>
  let bool: bool => t<bool>
  let if_: (t<bool>, t<'a>, t<'a>) => t<bool>
  let eq: (t<'a>, t<'a>) => t<bool>
  let plus: (t<int>, t<int>) => t<int>

  // Evaluation functions

  let int_eval: t<int> => int
  let bool_eval: t<bool> => bool
}

module Typesafe_lang: Typesafe_lang_sig = {
  // the type parameter 'a is the phantom type, since it doesn’t show up in the body of the
  // definition (signature)  of t.
  type t<'a> = expr // <-- this is called phantom type.

  let int = x => Value(Int(x))
  let bool = x => Value(Bool(x))
  let if_ = (c, t, e) => If(c, t, e)
  let eq = (x, y) => Eq(x, y)
  let plus = (x, y) => Plus(x, y)

  let int_eval = expr => {
    switch eval(expr) {
    | Int(x) => x
    | Bool(_) => raise(Ill_typed)
    }
  }

  let bool_eval = expr => {
    switch eval(expr) {
    | Bool(x) => x
    | Int(_) => raise(Ill_typed)
    }
  }
}

{
  open Typesafe_lang
  plus(int(3), int(2))->Js.log // ok, types match
  // let expr = plus(int(3), bool(true)) // compiler error, GOOD, type safe.

  // As you can see, the ill-typed expression we had trouble with before can’t be constructed,
  //  because it’s rejected by OCaml’s type-system.

  // So, what happened here? How did we add the type-safety we wanted? The fundamental
  // trick is to add what’s called a phantom type. In this definition:
  //
  // type t<'a> = expr
  //
  //  the type parameter 'a is the phantom type, since it doesn’t show up in the body of the definition of t.

  //  Because the type parameter is unused, it’s free to take on any value. That means we can
  //  constrain the use of that type parameter arbitrarily in the signature, which is a freedom we use
  //  to add the type-safety rules that we wanted.
}

//
// Trying to Do Better with Ordinary Variants
//
"Try_Ordinary_Variants"->Js.log

module Try_Ordinary_Variants = {
  type value<'a> =
    | Int('a)
    | Bool('a)

  type rec expr<'a> =
    | Value(value<'a>)
    | Eq(expr<'a>, expr<'a>)
    | Plus(expr<'a>, expr<'a>)
    | If(expr<bool>, expr<'a>, expr<'a>)

  // This looks promising at first, but it doesn’t quite do what we want. Let’s experiment a little.
  let i = x => Value(Int(x))
  let b = x => Value(Bool(x))
  let \"+" = (x, y) => Plus(x, y)
  i(3)->Js.log
  b(false)->Js.log
  (i(3) + i(2))->Js.log
  b(3)->Js.log // how can a bool be an int? doesn't type check well with this implementation

  // The problem here is that the way we want to use the type parameter isn’t supported by
  // ordinary variants. In particular, we want the type parameter to be populated in different ways
  // in the different tags, and to depend in non-trivial ways on the types of the data associated with
  // each tag. That’s where GADTs can help.
}

//
// GADTs to the Rescue
//
// Now we’re ready to write our first GADT. Here’s a new version of our value and expr
// types that correctly encode our desired typing rules.
//
"GADTs"->Js.log

module GADTs = {
  // this is how to contruct a GADT
  // notice the use of return type of value<int> in the type constructor Int(int)
  // think of it as function, input type is int and constructed type is value<int>
  type rec value<_> =
    | Int(int): value<int>
    | Bool(bool): value<bool>

  type rec expr<_> =
    | Value(value<'a>): expr<'a>
    | Eq(expr<int>, expr<int>): expr<bool>
    | Plus(expr<int>, expr<int>): expr<int>
    | If(expr<bool>, expr<'a>, expr<'a>): expr<'a>

  {
    let i = x => Value(Int(x))
    let b = x => Value(Bool(x))
    let \"+" = (x, y) => Plus(x, y)

    i(3)->Js.log
    // b(3)->Js.log // This won't compile since it is type safe with GADT.
    (i(3) + i(6))->Js.log
    //     i(3) + b(false) // This won't compile since it is type safe with GADT.

    // What we see here is that the type-safety rules we previously enforced with signature-level
    // restrictions on phantom types are now directly encoded in the definition of the expression type.

    // These type-safety rules apply not just when constructing an expression, but also when
    // deconstructing one, which means we can write a simpler and more concise evaluator that
    // doesn’t need any type-safety checks.
  }

  let eval_value:
    type a. value<a> => a =
    // notice this line.  type a. is existential type (locally abstract type)
    v => {
      switch v {
      | Int(x) => x
      | Bool(x) => x
      }
    }

  let rec eval:
    type a. expr<a> => a =
    e => {
      switch e {
      | Value(v) => eval_value(v)
      | If(c, t, e) => eval(c) ? eval(t) : eval(e)
      | Eq(x, y) => eval(x) == eval(y)
      | Plus(x, y) => eval(x) + eval(y)
      }
    }

  // Note that we now have a single polymorphic eval function, as opposed to the two
  //  type-specific evaluators we needed when using phantom types.
}
