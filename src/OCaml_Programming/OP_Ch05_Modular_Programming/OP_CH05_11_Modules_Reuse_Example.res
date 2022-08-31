// Exercise: refactor arith [★★★★]
//
let log = Js.log
let log2 = Js.log2

//
// Ring
//

/**
  Ring Base Interface
*/
module type Ring_Base = {
  type t

  let zero: t
  let one: t
  let \"+": (t, t) => t
  let \"~-": t => t
  let \"*": (t, t) => t
  let to_string: t => string
}

/**
  OfInt: interface for specific op
*/
module type OfInt_Op = {
  type t
  let of_int: int => t
}

/**
  Ring Interface - base plus OfInt
*/
module type Ring = {
  include Ring_Base
  include OfInt_Op with type t := t
}

/**
  Functor to make a Ring given an implementation of Ring_Base_Op
*/
module Ring_Make = (R: Ring_Base): (Ring with type t = R.t) => {
  include R
  open R
  let of_int = n => {
    let two = one + one
    let rec loop = (n, b, x) => {
      n == 0 ? x : loop(Pervasives.\"/"(n, 2), b * two, mod(n, 2) == 0 ? x : x + b)
    }
    let m = loop(abs(n), one, zero)
    n < 0 ? -m : m
  }
}

/**
  Int implementation of Ring_Base_Op
*/
module Ring_Base_Int = {
  type t = int
  let zero = 0
  let one = 1
  let \"+" = \"+"
  let \"~-" = \"~-"
  let \"*" = \"*"
  let to_string = string_of_int
}

/**
  Concrete implementation a Ring of Int
*/
module IntRing: Ring = Ring_Make(Ring_Base_Int)

/**
  Float implementation of Ring_Base_Op
  */
module Ring_Base_Float = {
  type t = float

  let zero = 0.
  let one = 1.
  let \"+" = \"+."
  let \"~-" = \"~-."
  let \"*" = \"*."
  let to_string = string_of_float
}

/**
  Concrete implementation a Ring of Float
*/
module FloatRing: Ring = Ring_Make(Ring_Base_Float)

let () = {
  IntRing.of_int(20)->log2("IntRing.of_int", _)
  FloatRing.of_int(30)->log2("FloatRing.of_int", _)
}

//
// Field
//

/**
  Field Base Interface - extends Ring Base
*/
module type Field_Base = {
  include Ring_Base
  let \"/": (t, t) => t
}

/**
  Field Interface - base plus OfInt
*/
module type Field = {
  include Field_Base
  include OfInt_Op with type t := t
}

/**
  Functor to make a Field given an implementation of Field_Base_Op
*/
module Field_Make = (F: Field_Base): Field => {
  // first make a Ring with a specific instance of OfInt_Op
  module R: OfInt_Op with type t := F.t = Ring_Make(F)
  include F
  include R
}

module Field_Base_Int = {
  include Ring_Base_Int
  let \"/" = \"/"
}

module IntField = Field_Make(Field_Base_Int)

module Field_Base_Float = {
  include Ring_Base_Float
  let \"/" = \"/."
}

module FloatField = Field_Make(Field_Base_Float)

let () = {
  IntField.of_int(20)->log2("IntField.of_int", _)
  FloatField.of_int(30)->log2("FloatField.of_int", _)
}

/**
 Rational Fields
*/
module Fraction = (F: Field) => {
  type t = (F.t, F.t)
  let zero = (F.zero, F.one)
  let one = (F.one, F.one)
  let \"+" = ((a, b), (c, d)) => {
    open F
    (a * d + c * b, b * d)
  }
  let \"~-" = ((a, b)) => {
    (F.\"~-"(a), b)
  }

  let \"*" = ((a, b), (c, d)) => {
    (F.\"*"(a, c), F.\"*"(b, d))
  }
  let \"/" = ((a, b), (c, d)) => {
    (a, b) * (d, c)
  }

  let to_string = ((a, b)) => {
    open F
    F.to_string(a) ++ "/" ++ F.to_string(b)
  }
}

module IntRational: Field = Field_Make(Fraction(IntField))
module FloatRational: Field = Field_Make(Fraction(FloatField))
