// https://cs3110.github.io/textbook/chapters/modules/module_type_constraints.html
//

// As a motivating example, here is a module type that represents values that support the usual
// addition and multiplication operations from arithmetic, or more precisely, a ring:

let log = Js.log
module START = {
  module type Ring = {
    type t
    let zero: t
    let one: t
    let \"+": (t, t) => t
    let \"*": (t, t) => t
    let \"~-": t => t // additive inverse
    let to_string: t => string
  }

  module IntRing: Ring = {
    type t = int
    let zero = 0
    let one = 1
    let \"+" = Pervasives.\"+"
    let \"*" = Pervasives.\"*"
    let \"~-" = Pervasives.\"~-"
    let to_string = string_of_int
  }

  {
    open IntRing
    (one + one)->to_string->log
  }

  module FloatRing: Ring = {
    type t = float
    let zero = 0.
    let one = 1.
    let \"+" = Pervasives.\"+."
    let \"*" = Pervasives.\"*."
    let \"~-" = Pervasives.\"~-."
    let to_string = string_of_float
  }
}
// 5.7.1. Specializing Module Types#
//
// In the past, we’ve seen that we can leave off the module type annotation, then do a separate
// check to make sure the structure satisfies the signature:
//
//module R: Ring = IntRing

// There’s a more sophisticated way of accomplishing the same goal. We can specialize the Ring
// module type to specify that t must be int or float. We do that by adding a constraint using the
// with keyword:

module type Ring = {
  type t
  let zero: t
  let one: t
  let \"+": (t, t) => t
  let \"*": (t, t) => t
  let \"~-": t => t // additive inverse
  let to_string: t => string
}

module type INT_RING = Ring with type t = int

module IntRing: INT_RING = {
  type t = int
  let zero = 0
  let one = 1
  let \"+" = Pervasives.\"+"
  let \"*" = Pervasives.\"*"
  let \"~-" = Pervasives.\"~-"
  let to_string = string_of_int
}

// It turns out there’s no need to separately define INT_RING and FLOAT_RING. The with keyword can
// be used as part of the module definition, though the syntax becomes a little harder to read
// because of the proximity of the two = signs:

module FloatRing: Ring with type t = float = {
  type t = float
  let zero = 0.
  let one = 1.
  let \"+" = Pervasives.\"+."
  let \"*" = Pervasives.\"*."
  let \"~-" = Pervasives.\"~-."
  let to_string = string_of_float
}

// 5.7.2. Constraints
//
// Syntax.
//
// There are two sorts of constraints. One is the sort we saw above, with type equations:
//
// - T with type x = t, where T is a module type, x is a type name, and t is a type.
//
// The other sort is a module equation, which is syntactic sugar for specifying the equality of all
// types in the two modules:
//
// - T with module M = N, where M and N are module names.
//
// Multiple constraints can be added with the and keyword:
//
// - T with constraint1 and constraint2 and ... constraintN
//

// Static semantics.
//
// The constrained module type T with type x = t is the same as T, except that the declaration of
// type x inside T is replaced by type x = t. For example, compare the two signatures output below:

module type XY = {
  type x
  type y
}

module type T = {
  module A: XY
}

module B = {
  type x = int
  type y = float
}

module type U = T with module A = B

module C: U = {
  module A = {
    type x = int
    type y = float
    let _x = 42
  }
}

// Focus on the output for module type U. Notice that the types of x and y in it have become int
// and float because of the module A = B constraint. Also notice how modules B and C.A are not the
// same module; the latter has an extra item x in it. So the syntax module A = B is potentially
// confusing. The constraint is not specifying that the two modules are the same. Rather, it
// specifies that all their types are constrained to be equal.
