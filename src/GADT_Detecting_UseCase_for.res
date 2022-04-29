//
// https://blog.mads-hartmann.com/ocaml/2015/01/05/gadt-ocaml.html
//
// Detecting use-cases for GADTs in OCaml
// by Mads Hartmann - 05 Jan 2015
//
// [VERY GOOD READ]

// Using ADTs

module ADTs = {
  type value =
    | Bool(bool)
    | Int(int)

  type rec expr =
    | Value(value)
    | If(expr, expr, expr)
    | Eq(expr, expr)
    | Lt(expr, expr)

  let rec eval: expr => value = expr => {
    switch expr {
    | Value(v) => v
    | Lt(x, y) =>
      switch (eval(x), eval(y)) {
      | (Int(x), Int(y)) => Bool(x < y)
      | (Int(_), Bool(_))
      | (Bool(_), Int(_))
      | (Bool(_), Bool(_)) =>
        failwith("Invalid AST")
      }
    | If(b, l, r) =>
      switch eval(b) {
      | Bool(true) => eval(l)
      | Bool(false) => eval(r)
      | Int(_) => failwith("Invalid AST")
      }
    | Eq(a, b) =>
      switch (eval(a), eval(b)) {
      | (Int(x), Int(y)) => Bool(x == y)
      | (Bool(_), Bool(_))
      | (Bool(_), Int(_))
      | (Int(_), Bool(_)) =>
        failwith("Invalid AST")
      }
    }
    // symptom #1: When you need to to add extra cases for invalid states to make your pattern matches exhaustive
  }

  // symptom #2: You want the result of a function to depend on the data constructor used to create the data
  let eval_int: value => int = value => {
    switch value {
    | Int(x) => x
    | Bool(_) => failwith("Got Bool, expected Int")
    }
  }

  let eval_bool: value => bool = value => {
    switch value {
    | Bool(b) => b
    | Int(_) => failwith("Got Int, expected Bool")
    }
  }
}

ADTs.eval(If(Lt(Value(Int(2)), Value(Int(4))), Value(Int(42)), Value(Int(0))))->Js.log

//
// Using GADT
//
module GADT = {
  type rec value<_> =
    | Bool(bool): value<bool>
    | Int(int): value<int>

  type rec expr<_> =
    | Value(value<'a>): expr<'a>
    | If(expr<bool>, expr<'a>, expr<'a>): expr<'a>
    | Eq(expr<'a>, expr<'a>): expr<bool>
    | Lt(expr<int>, expr<int>): expr<bool>

  let rec eval:
    type a. expr<a> => a =
    expr => {
      switch expr {
      | Value(Bool(b)) => b
      | Value(Int(i)) => i
      | If(b, l, r) => eval(b) ? eval(l) : eval(r)
      | Eq(a, b) => eval(a) == eval(b)
      | Lt(a, b) => eval(a) < eval(b)
      }
    }
}

GADT.eval(If(Eq(Value(Int(2)), Value(Int(2))), Value(Int(42)), Value(Int(12))))->Js.log

GADT.eval(If(Eq(Value(Int(2)), Value(Int(2))), Value(Bool(true)), Value(Bool(false))))->Js.log

//
// caught the error at compile time!!
//
// GADT.eval(If(Value(Int(42)), Value(Int(42)), Value(Int(42))))->Js.log
//
// Error: This expression has type GADT.value<int>
//       but an expression was expected of type GADT.value<bool>
//       Type int is not compatible with type bool
