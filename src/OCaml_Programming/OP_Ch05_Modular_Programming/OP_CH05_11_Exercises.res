// https://cs3110.github.io/textbook/chapters/modules/exercises.html
//
let log = Js.log
let log2 = Js.log2

// Exercise: complex synonym [★]
//
module Complex_Synonym = {
  // Here is a module type for complex numbers, which have a real and imaginary component:
  module type ComplexSig_Q = {
    let zero: (float, float)
    let add: ((float, float), (float, float)) => (float, float)
  }
  // Improve that code by adding type t = float * float. Show how the signature can be written more
  // tersely because of the type synonym.

  module type ComplexSig_A = {
    type t = (float, float)
    let zero: t
    let add: (t, t) => t
  }
}

// Exercise: complex encapsulation [★★]
//
module Complex_Encapsulation = {
  // Here is a module for the module type from the previous exercise:
  module Complex: Complex_Synonym.ComplexSig_A = {
    type t = (float, float)
    let zero = (0., 0.)
    let add = ((r1, i1), (r2, i2)) => (r1 +. r2, i1 +. i2)
  }

  // Investigate what happens if you make the following changes (each independently), and explain
  // why any errors arise:
  //
  //remove zero from the structure
  //remove add from the signature
  //change zero in the structure to let zero = 0, 0
}

// Exercise: big list queue [★★]
//
module Big_List_Queue = {
  // Use the following code to create ListQueue of exponentially increasing length: 10, 100, 1000,
  // etc. How big of a queue can you create before there is a noticeable delay? How big until
  // there’s a delay of at least 10 seconds? (Note: you can abort utop computations with Ctrl-C.)

  module ListQueue = OP_ListQueue.M

  let fill_listqueue = n => {
    let rec loop = (n, q) => {
      n == 0 ? q : loop(n - 1, ListQueue.enqueue(n, q))
    }
    loop(n, ListQueue.empty)
  }
}

// Exercise: fraction [★★★]
//

module Fraction = {
  // Write a module that implements the Fraction module type below:
  module type Fraction = {
    //  (* A fraction is a rational number p/q, where q != 0.*)
    type t

    //  (** [make n d] is n/d. Requires d != 0. *)
    let make: (int, int) => t
    let numerator: t => int
    let denominator: t => int
    let to_string: t => string
    let to_float: t => float
    let add: (t, t) => t
    let mul: (t, t) => t
  }

  exception Fraction_Exn(string)

  module Fraction: Fraction = {
    type t = {p: int, q: int}

    let make = (p, q) => {
      if q == 0 {
        raise(Fraction_Exn("Division by zero"))
      } else {
        {p, q}
      }
    }
    let numerator = t => t.p
    let denominator = t => t.q
    let to_string = ({p, q}) => {j`$p / $q`}
    let to_float = ({p, q}) => float(p) /. float(q)
    let add = ({p: p1, q: q1}, {p: p2, q: q2}) => {
      let p = p1 * q2 + p2 * q1
      let q = q1 * q2
      make(p, q)
    }
    let mul = ({p: p1, q: q1}, {p: p2, q: q2}) => {
      let p = p1 * p2
      let q = q1 * q2
      make(p, q)
    }
  }
}

// Exercise: fraction reduced [★★★]
//
module Fraction_Reduced = {
  // Modify your implementation of Fraction to ensure these invariants hold of every value v of type t
  // that is returned from make, add, and mul:

  let rec gcd = (x, y) => {
    x == 0 ? y : x < y ? gcd(y - x, x) : gcd(y, x - y)
  }

  module Fraction: Fraction.Fraction = {
    type t = {p: int, q: int}

    let make = (p, q) => {
      if q == 0 {
        raise(Fraction.Fraction_Exn("Division by zero"))
      } else if q < 0 {
        raise(Fraction.Fraction_Exn("Denominator must be positive"))
      } else {
        let g = gcd(p, q)
        {p: p / g, q: q / g}
      }
    }
    let numerator = t => t.p
    let denominator = t => t.q
    let to_string = ({p, q}) => {j`$p / $q`}
    let to_float = ({p, q}) => float(p) /. float(q)
    let add = ({p: p1, q: q1}, {p: p2, q: q2}) => {
      let p = p1 * q2 + p2 * q1
      let q = q1 * q2
      make(p, q)
    }
    let mul = ({p: p1, q: q1}, {p: p2, q: q2}) => {
      let p = p1 * p2
      let q = q1 * q2
      make(p, q)
    }
  }

  open Fraction
  make(12, 6)->to_string->log
}
