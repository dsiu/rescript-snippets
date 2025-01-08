module type SEMIGROUP = {
  type t
  let append: t => t => t
}

// Semigroups must satisfy associativity
// eg:
// append(a, append(b, c)) = append(append(a, b), c)
// a->append(b->append(c)) = a->append(b)->append(c)

// More formally, a semigroup is a pair of type t and a binary associative operation where the
// operands and the result all have the same type t.

// Examples
module IntAdd = {
  type t = int
  let append = \"+"
}

module IntMul = {
  type t = int
  let append = \"*"
}

module BoolAnd = {
  type t = bool
  let append = \"&&"
}

module BoolOr = {
  type t = bool
  let append = \"||"
}

// String concatenation, in turn, is not commutative. The order of appending strings matters. But
// the operation is still associative.
// eg:
// String.append("abc", "def") != String.append("def", "abc")
module String = {
  type t = string
  let append = String.concat
}

// Counterexample
module IntSub = {
  type t = int
  let append = \"-"
}
