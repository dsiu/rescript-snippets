//
// https://dev.realworldocaml.org/functors.html
//
// Functors
//

// A Trivial Example
@@uncurried
@@uncurried.swap

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
    let compare: (. t, t) => int
  }

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

  module My_Str = {
    type t
    let compare = (. _, _) => 0
  }

  module My_Str_interval = Make_interval(My_Str)

  let i1 = Int_interval.create(3, 8)
  let i2 = Int_interval.create(4, 10)
  Int_interval.intersect(i1, i2)->Js.log

  module Rev_int_interval = Make_interval({
    type t = int
    let compare = (. x, y) => x - y
  })

  Int_interval.Interval(4, 3)->Int_interval.is_empty->Js.log // NOT good, should be able to access Interval.  see below for adding constraints
}
//
// Making the Functor Abstract
//

// There’s a problem with Make_interval. The code we wrote depends on the invariant that the
// upper bound of an interval is greater than its lower bound, but that invariant can be
// violated. The invariant is enforced by the create function, but because Int_interval.t
// is not abstract, we can bypass the create function:
module Sharing_Constraints = {
  module type Comparable = {
    type t
    let compare: (. t, t) => int
  }

  // To make Int_interval.t abstract, we need to restrict the output of Make_interval with
  // an interface. Here’s an explicit interface that we can use for that purpose:
  module type Interval_intf = {
    type t
    type endpoint
    let create: (endpoint, endpoint) => t
    let is_empty: t => bool
    let contains: (t, endpoint) => bool
    let intersect: (t, t) => t
  }

  module Make_interval = (Endpoint: Comparable): (
    Interval_intf with type endpoint = Endpoint.t
  ) => {
    type endpoint = Endpoint.t

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

  module type Int_Interval_inf = Interval_intf with type endpoint = int

  module Int_interval = Make_interval({
    type t = int
    let compare = Pervasives.compare
  })

  let i = Int_interval.create(3, 4)
  Int_interval.contains(i, 5)->Js.log
}

module Destructive_Substitution = {
  module type Comparable = {
    type t
    let compare: (. t, t) => int
  }

  // To make Int_interval.t abstract, we need to restrict the output of Make_interval with
  // an interface. Here’s an explicit interface that we can use for that purpose:
  module type Interval_intf = {
    type t
    type endpoint
    let create: (endpoint, endpoint) => t
    let is_empty: t => bool
    let contains: (t, endpoint) => bool
    let intersect: (t, t) => t
  }

  module type Int_Interval_inf = Interval_intf with type endpoint := int

  module Make_interval = (Endpoint: Comparable): (
    Interval_intf with type endpoint := Endpoint.t
  ) => {
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
  //The interface is precisely what we want: the type t is abstract, and the type of the endpoint
  // is exposed; so we can create values of type Int_interval.t using the creation function, but
  // not directly using the constructors and thereby violating the invariants of the module.

  module Int_interval = Make_interval({
    type t = int
    let compare = Pervasives.compare
  })

  Int_interval.create(3, 4)->Int_interval.is_empty->Js.log
  // Int_interval.Interval(4, 3)->Int_interval.is_empty->Js.log // can't do it, good.
}

module Using_Multiple_Interfaces = {
  module Sexp = {
    type t
  }

  module Sexpable = {
    module type S = {
      type t
      let sexp_of_t: t => Sexp.t
      let t_of_sexp: Sexp.t => t
    }
  }

  module type Comparable = {
    type t
    let compare: (t, t) => int
  }

  // To make Int_interval.t abstract, we need to restrict the output of Make_interval with
  // an interface. Here’s an explicit interface that we can use for that purpose:
  module type Interval_intf = {
    type t
    type endpoint
    let create: (endpoint, endpoint) => t
    let is_empty: t => bool
    let contains: (t, endpoint) => bool
    let intersect: (t, t) => t
  }

  module type Interval_intf_with_sexp = {
    include Interval_intf
    include Sexpable.S with type t := t
  }

  module Make_interval = (
    Endpoint: {
      type t
      include Comparable with type t := t
      include Sexpable.S with type t := t
    },
  ): (Interval_intf with type endpoint := Endpoint.t) => {
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
}

//
// Extending Modules
//

module Extending_Modules = {}
