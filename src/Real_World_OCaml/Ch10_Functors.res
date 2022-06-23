//
// https://dev.realworldocaml.org/functors.html
//
// Functors
//

// A Trivial Example

module A_Trivial_Example = {
  module type X_int = {
    let x: int
  }

  // this is module functor
  module Increment = (M: X_int) => {
    let x = M.x + 1
  }

  // We can use Increment to define new modules:
  module Three = {
    let x = 3
  }

  module Four = Increment(Three)

  (Four.x - Three.x)->Js.log

  module Three_and_more = {
    let x = 3
    let y = "three"
  }

  module Four_1 = Increment(Three_and_more)
}

//
// A Bigger Example: Computing with Intervals
//
module A_Bigger_Example = {
  module type Comparable = {
    type t
    let compare: (t, t) => int
  }

  // compare x y < 0     (* x < y *)
  // compare x y = 0     (* x = y *)
  // compare x y > 0     (* x > y *)

  module Make_interval = (Endpoint: Comparable) => {
    type t =
      | Interval(Endpoint.t, Endpoint.t)
      | Empty

    // [create low high] creates a new interval from [low] to
    // [high].  If [low > high], then the interval is empty
    let create = (low, high) => {
      Endpoint.compare(low, high) > 0 ? Empty : Interval(low, high)
    }

    // Returns true iff the interval is empty
    let is_empty = x => {
      switch x {
      | Empty => true
      | Interval(_) => false
      }
    }

    // [contains t x] returns true iff [x] is contained in the
    // interval [t]
    let contains = (t, x) => {
      switch t {
      | Empty => false
      | Interval(l, h) => Endpoint.compare(x, l) >= 0 && Endpoint.compare(x, h) <= 0
      }
    }

    // [intersect t1 t2] returns the intersection of the two input
    // intervals
    let intersect = (t1, t2) => {
      let min = (x, y) => Endpoint.compare(x, y) <= 0 ? x : y
      let max = (x, y) => Endpoint.compare(x, y) >= 0 ? x : y
      switch (t1, t2) {
      | (Empty, _)
      | (_, Empty) =>
        Empty
      | (Interval(l1, h1), Interval(l2, h2)) => create(max(l1, l2), min(h1, h2))
      }
    }
  }

  // We can instantiate the functor by applying it to a module with the right signature. In the
  // following code, rather than name the module first and then call the functor, we provide the
  // functor input as an anonymous module:
  module Int_interval = Make_interval({
    type t = int
    let compare = Pervasives.compare
  })

  module String_interval = Make_interval({
    type t = string
    let compare = Pervasives.compare
  })
}
